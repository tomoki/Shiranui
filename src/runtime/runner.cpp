#include "runner.hpp"
#include "dsl/dsl_runtime.hpp"
#include <iostream>
#include <typeinfo>
#include <memory>
#include <boost/thread/thread.hpp>

#define BEFORE_VISIT_MACRO(NODE) int cur_t_ = before_visit(NODE)
#define AFTER_VISIT_MACRO(NODE) return after_visit(NODE, cur_t_)
namespace shiranui {
    namespace runtime {
        template<typename T,typename S>
        std::map<T,S> merge_map(const std::map<T,S>& a,const std::map<T,S>& b){
            std::map<T,S> ret;
            for(const auto& c : {a,b}){
                for(const auto& p : c){
                    ret[p.first] = p.second;
                }
            }
            return ret;
        }
        using shiranui::runtime::environment::Environment;
        using shiranui::runtime::value::Value;
        using shiranui::runtime::value::Integer;
        using shiranui::runtime::value::String;
        using shiranui::runtime::value::Array;
        using shiranui::runtime::value::Boolean;
        using shiranui::runtime::value::Function;
        using shiranui::runtime::value::UserFunction;
        using shiranui::runtime::value::Return;
        using shiranui::runtime::value::SystemCall;
        using shiranui::runtime::value::BuiltinFunction;
        using shiranui::runtime::value::Ref;
        using namespace shiranui::runtime::value::builtin;
        Runner::Runner(Memory* memory_, bool is_server_)
            : memory(memory_),
              cur_v(memory->create<Integer>(0)),
              cur_e(memory->create<Environment>(memory)),
              cur_t(0),
              call_depth(0),
              is_server(is_server_){
            call_stack.push(infomation::TOPLEVEL); // -1 is toplevel
            call_stack_block.push(nullptr);
        }
        template <typename T>
        int Runner::before_visit(T &node) {
            boost::this_thread::interruption_point();
            cur_t++;
            call_depth++;
            if(call_depth > 10000){
                throw MaxDepthExceededException(&(node));
            }
            if (is_server) {
                node.runtime_info.visit_time.push_back(cur_t);
                node.runtime_info.call_under[call_stack.top()] = cur_t;
            }
            return cur_t;
        }

        template <typename T>
        void Runner::after_visit(T &node, int cur_t_) {
            using namespace infomation;
            if (is_server) {
                // store version_info too.
                node.runtime_info.return_value[cur_t_] = make_return_value(cur_v);
            }
            call_depth--;
            boost::this_thread::interruption_point();
        }
        void Runner::push_callstack(int id, sp<syntax::ast::Block> b){
            if(call_stack.top() != infomation::TOPLEVEL){
                b->runtime_info.up[id] = std::make_pair(call_stack.top(),call_stack_block.top());
            }
            call_stack.push(id);
            call_stack_block.push(b);
        }
        void Runner::pop_callstack(){
            call_stack.pop();
            call_stack_block.pop();
        }
        void Runner::visit(syntax::ast::Identifier &) {
            throw RuntimeException(); // never occur.
            return;
        }
        void Runner::visit(syntax::ast::Variable &var) {
            BEFORE_VISIT_MACRO(var);
            if (var.value.name == "system_call") {
                cur_v = memory->create<SystemCall>();
                AFTER_VISIT_MACRO(var);
            }
            if (cur_e->has(var.value)) {
                cur_v = cur_e->get(var.value);
                AFTER_VISIT_MACRO(var);
            } else {
                throw NoSuchVariableException(&(var));
            }
        }
        void Runner::visit(syntax::ast::Number &num) {
            BEFORE_VISIT_MACRO(num);
            cur_v = memory->create<Integer>(num.value);
            AFTER_VISIT_MACRO(num);
        }
        void Runner::visit(syntax::ast::String &s) {
            BEFORE_VISIT_MACRO(s);
            cur_v = memory->create<String>(s.value);
            AFTER_VISIT_MACRO(s);
        }
        void Runner::visit(syntax::ast::Boolean& node){
            BEFORE_VISIT_MACRO(node);
            cur_v = memory->create<Boolean>(node.value);
            AFTER_VISIT_MACRO(node);
        }
        void Runner::visit(syntax::ast::Enum &e) {
            BEFORE_VISIT_MACRO(e);
            std::vector<sp<Value>> vs;
            for (sp<syntax::ast::Expression> exp : e.expressions) {
                exp->accept(*this);
                vs.push_back(cur_v);
            }
            cur_v = memory->create<Array>(vs);
            AFTER_VISIT_MACRO(e);
        }
        void Runner::visit(syntax::ast::Interval &intr) {
            BEFORE_VISIT_MACRO(intr);
            sp<Value> start = nullptr, end = nullptr, next = nullptr;
            {
                intr.start->accept(*this);
                start = cur_v;
                intr.end->accept(*this);
                end = cur_v;
                if (intr.next != nullptr) {
                    intr.next->accept(*this);
                    next = cur_v;
                }
            }
            {
                sp<Integer> s = dynamic_cast<Integer*>(start);
                sp<Integer> e = dynamic_cast<Integer*>(end);
                if (s != nullptr and e != nullptr) {
                    int change = 0;
                    if (next != nullptr) {
                        sp<Integer> n = dynamic_cast<Integer*>(next);
                        if (n == nullptr) {
                            throw ConvertException(&(intr));
                        }
                        change = n->value - s->value;
                        if (change == 0) {
                            throw RangeException(&(intr));
                        }
                    }
                    std::vector<sp<Value>> vs;
                    if (s->value <= e->value and change >= 0) {
                        // if next == nullptr
                        if (change == 0)
                            change = 1;
                        if (intr.right_close) {
                            for (int i = s->value; i <= e->value; i += change) {
                                boost::this_thread::interruption_point();
                                vs.push_back(memory->create<Integer>(i));
                            }
                        } else {
                            for (int i = s->value; i < e->value; i += change) {
                                boost::this_thread::interruption_point();
                                vs.push_back(memory->create<Integer>(i));
                            }
                        }
                    } else if (s->value >= e->value and change < 0) {
                        // if next == nullptr
                        if (intr.right_close) {
                            for (int i = s->value; i >= e->value; i += change) {
                                boost::this_thread::interruption_point();
                                vs.push_back(memory->create<Integer>(i));
                            }
                        } else {
                            for (int i = s->value; i > e->value; i += change) {
                                boost::this_thread::interruption_point();
                                vs.push_back(memory->create<Integer>(i));
                            }
                        }
                    }
                    cur_v = memory->create<Array>(vs);
                    AFTER_VISIT_MACRO(intr);
                }
            }
            throw ConvertException(&(intr));
        }

        void Runner::visit(syntax::ast::Block &block) {
            BEFORE_VISIT_MACRO(block);
            sp<Environment> before = cur_e;
            sp<Environment> inner = memory->create<Environment>(memory,cur_e);
            cur_e = inner;
            for (auto f : block.pre) {
                f->accept(*this);
            }
            for (auto f : block.invariant) {
                f->accept(*this);
            }
            for(size_t s=0,f=0;s<block.statements.size()
                            or f<block.flymarks.size();){
                if(s == block.statements.size()){
                    block.flymarks[f]->accept(*this);
                    f++;continue;
                }
                if(f == block.flymarks.size()){
                    block.statements[s]->accept(*this);
                    sp<Return> r = dynamic_cast<Return*>(cur_v);
                    if (r != nullptr) {
                        break;
                    }
                    s++;continue;
                }
                if(block.statements[s]->point < block.flymarks[f]->point){
                    block.statements[s]->accept(*this);
                    sp<Return> r = dynamic_cast<Return*>(cur_v);
                    if (r != nullptr) {
                        break;
                    }
                    s++;continue;
                }else{
                    block.flymarks[f]->accept(*this);
                    f++;continue;
                }
            }

            sp<Value> before_post_execute = cur_v;
            sp<Return> ret = dynamic_cast<Return*>(cur_v);
            bool last_was_return = ret != nullptr;

            for (size_t i = 0; i < block.post.size(); i++) {
                if (last_was_return) {
                    syntax::ast::Identifier id = block.post_id[i];
                    if (id.name == "") {
                        throw ConvertException(&(block));
                    }
                    sp<Environment> return_added = memory->create<Environment>(memory,cur_e);
                    // convert Return to Value
                    return_added->define(id, ret->value);
                    cur_e = return_added;
                    block.post[i]->accept(*this);
                    cur_e = return_added->parent;
                } else {
                    block.post[i]->accept(*this);
                }
            }
            for (sp<shiranui::syntax::ast::Block> f : block.invariant) {
                f->accept(*this);
            }
            cur_v = before_post_execute;
            cur_e = before;
            AFTER_VISIT_MACRO(block);
        }
        void Runner::visit(syntax::ast::Function &f) {
            BEFORE_VISIT_MACRO(f);
            cur_v = memory->create<UserFunction>(f.parameters, f.body, cur_e);
            AFTER_VISIT_MACRO(f);
        }
        void Runner::visit(syntax::ast::FunctionCall &fc) {
            BEFORE_VISIT_MACRO(fc);
            fc.function->accept(*this);
            sp<Value> func = cur_v;
            // check func.v is really function.
            {
                sp<UserFunction> f = dynamic_cast<UserFunction*>(func);
                if (f != nullptr) {
                    if (fc.arguments.size() != f->parameters.size()) {
                        throw ConvertException(&(fc));
                    }
                    sp<Environment> before = this->cur_e;
                    sp<Environment> call_env = memory->create<Environment>(memory,f->env);
                    for (int i = 0; i < static_cast<int>(f->parameters.size()); i++) {
                        fc.arguments[i]->accept(*this);
                        call_env->define(f->parameters[i], cur_v);
                    }
                    push_callstack(cur_t_,f->body);
                    this->cur_e = call_env;
                    f->body->accept(*this);
                    this->cur_e = before;
                    pop_callstack();
                    sp<Return> ret = dynamic_cast<Return*>(this->cur_v);
                    if (ret == nullptr) {
                        cur_v = this->cur_v; // does not require
                    } else {
                        cur_v = ret->value;
                    }
                    AFTER_VISIT_MACRO(fc);
                }
            }
            {
                sp<SystemCall> f = dynamic_cast<SystemCall*>(func);
                if (f != nullptr) {
                    if (fc.arguments.size() != 1) {
                        throw ConvertException(&(fc));
                    }
                    fc.arguments[0]->accept(*this);
                    sp<String> s = dynamic_cast<String*>(cur_v);
                    if (s == nullptr) {
                        throw ConvertException(&(fc));
                    } else {
                        if(s->value == "print"){
                            cur_v = memory->create<PrintFunction>(memory);
                        }else if(s->value == "length"){
                            cur_v = memory->create<LengthFunction>(memory);
                        }else if(s->value == "get"){
                            cur_v = memory->create<GetIndex>(memory);
                        }else if(s->value == "set"){
                            cur_v = memory->create<SetIndex>(memory);
                        }else{
                            throw ConvertException(&(fc));
                        }
                    }
                    AFTER_VISIT_MACRO(fc);
                }
            }
            {
                sp<BuiltinFunction> f = dynamic_cast<BuiltinFunction*>(func);
                if (f != nullptr) {
                    {
                        std::vector<sp<Value>> arguments;
                        for (sp<syntax::ast::Expression> arg : fc.arguments) {
                            arg->accept(*this);
                            arguments.push_back(cur_v);
                        }
                        sp<Value> ret = f->run(arguments);
                        if (ret == nullptr) {
                            throw ConvertException(&(fc));
                        } else {
                            cur_v = ret;
                        }
                        AFTER_VISIT_MACRO(fc);
                    }
                }
            }
            throw ConvertException(&(fc));
        }
        void Runner::visit(syntax::ast::BinaryOperator &bop) {
            BEFORE_VISIT_MACRO(bop);
            bop.left->accept(*this);
            sp<Value> left = cur_v;
            bop.right->accept(*this);
            sp<Value> right = cur_v;
            // equality check is defined in value.hpp.
            //  because it is used in server.
            if(bop.op == "="){
                cur_v = memory->create<Boolean>(check_equality(left,right));
                return;
            }else if(bop.op == "/="){
                cur_v = memory->create<Boolean>(not check_equality(left, right));
                return;
            }
            if (typeid(*left) != typeid(*right)) {
                throw ConvertException(&(bop));
            }
            {
                sp<Integer> l = dynamic_cast<Integer*>(left);
                sp<Integer> r = dynamic_cast<Integer*>(right);
                if (l != nullptr and r != nullptr) {
                    if (bop.op == "<") {
                        cur_v = memory->create<Boolean>(l->value < r->value);
                    } else if (bop.op == "<=") {
                        cur_v = memory->create<Boolean>(l->value <= r->value);
                    } else if (bop.op == ">") {
                        cur_v = memory->create<Boolean>(l->value > r->value);
                    } else if (bop.op == ">=") {
                        cur_v = memory->create<Boolean>(l->value >= r->value);
                    } else if (bop.op == "+") {
                        cur_v = memory->create<Integer>(l->value + r->value);
                    } else if (bop.op == "-") {
                        cur_v = memory->create<Integer>(l->value - r->value);
                    } else if (bop.op == "*") {
                        cur_v = memory->create<Integer>(l->value * r->value);
                    } else if (bop.op == "/") {
                        if (r->value == 0) {
                            throw ZeroDivException(&(bop));
                        }
                        cur_v = memory->create<Integer>(l->value / r->value);
                    } else if (bop.op == "%") {
                        cur_v = memory->create<Integer>(l->value % r->value);
                    } else if (bop.op == "^") {
                    } else {
                        throw ConvertException(&(bop));
                    }
                    AFTER_VISIT_MACRO(bop);
                }
            }
            {
                sp<Boolean> l = dynamic_cast<Boolean*>(left);
                sp<Boolean> r = dynamic_cast<Boolean*>(right);
                if (l != nullptr and r != nullptr) {
                    if (bop.op == "and") {
                        cur_v = memory->create<Boolean>(l->value and r->value);
                    } else if (bop.op == "or") {
                        cur_v = memory->create<Boolean>(l->value or r->value);
                    } else {
                        throw ConvertException(&(bop));
                    }
                    AFTER_VISIT_MACRO(bop);
                }
            }
            {
                sp<String> l = dynamic_cast<String*>(left);
                sp<String> r = dynamic_cast<String*>(right);
                if (l != nullptr and r != nullptr) {
                    if (bop.op == "+") {
                        cur_v = memory->create<String>(l->value + r->value);
                    } else {
                        throw ConvertException(&(bop));
                    }
                    AFTER_VISIT_MACRO(bop);
                }
            }
            throw ConvertException(&(bop));
            // if (bop.op == "+") {
            // } else if (bop.op == "-") {
            // } else if (bop.op == "*") {
            // } else if (bop.op == "/") {
            // } else if (bop.op == "%") {
            // } else if (bop.op == "^") {
            // } else if (bop.op == "and") {
            // } else if (bop.op == "or") {
            // }
        }
        void Runner::visit(syntax::ast::UnaryOperator &uop) {
            BEFORE_VISIT_MACRO(uop);
            uop.exp->accept(*this);
            sp<Value> v_ = cur_v;
            {
                sp<Integer> v = dynamic_cast<Integer*>(v_);
                if (v != nullptr) {
                    if (uop.op == "+") {
                        cur_v = v;
                        return;
                    } else if (uop.op == "-") {
                        cur_v = memory->create<Integer>(-(v->value));
                        AFTER_VISIT_MACRO(uop);
                    }
                }
            }
            {
                sp<Boolean> v = dynamic_cast<Boolean*>(v_);
                if (v != nullptr) {
                    if (uop.op == "not") {
                        cur_v = memory->create<Boolean>(not(v->value));
                        AFTER_VISIT_MACRO(uop);
                    }
                }
            }
            {
                sp<Ref> v = dynamic_cast<Ref*>(v_);
                if(v != nullptr){
                    if(uop.op == "!"){
                        cur_v = v->to;
                        AFTER_VISIT_MACRO(uop);
                    }
                }
            }
            {
                if(uop.op == "ref"){
                    cur_v = memory->create<Ref>(cur_v);
                    AFTER_VISIT_MACRO(uop);
                }
            }

            throw ConvertException(&(uop));
        }
        void Runner::visit(syntax::ast::IfElseExpression &iee) {
            BEFORE_VISIT_MACRO(iee);
            AFTER_VISIT_MACRO(iee);
        }
        void Runner::visit(syntax::ast::Definement &def) {
            BEFORE_VISIT_MACRO(def);
            def.value->accept(*this);
            if(def.is_const){
                cur_e->define(def.id,cur_v);
            }else{
                cur_e->define(def.id,memory->create<Ref>(cur_v));
            }
            AFTER_VISIT_MACRO(def);
        }
        void Runner::visit(syntax::ast::ExpressionStatement& es){
            BEFORE_VISIT_MACRO(es);
            es.exp->accept(*this);
            AFTER_VISIT_MACRO(es);
        }
        void Runner::visit(syntax::ast::ReturnStatement &ret) {
            BEFORE_VISIT_MACRO(ret);
            ret.value->accept(*this);
            cur_v = memory->create<Return>(cur_v);
            AFTER_VISIT_MACRO(ret);
        }
        void Runner::visit(syntax::ast::ProbeStatement &probe) {
            BEFORE_VISIT_MACRO(probe);
            probe.value->accept(*this);
            // cur_v = std::make_shared<Return>(cur_v);
            AFTER_VISIT_MACRO(probe);
        }
        void Runner::visit(syntax::ast::AssertStatement &as) {
            BEFORE_VISIT_MACRO(as);
            as.value->accept(*this);
            sp<Boolean> bp = dynamic_cast<Boolean*>(cur_v);
            if (bp == nullptr) {
                throw ConvertException(as.value);
            } else {
                if (bp->value) {
                } else {
                    throw AssertException(as.value);
                }
            }
            AFTER_VISIT_MACRO(as);
        }

        void Runner::visit(syntax::ast::IfElseStatement &ies) {
            BEFORE_VISIT_MACRO(ies);
            ies.pred->accept(*this);
            sp<Boolean> bp = dynamic_cast<Boolean*>(cur_v);
            if (bp == nullptr) {
                throw ConvertException(ies.pred);
            }
            if (bp->value) {
                ies.ifblock->accept(*this);
            } else {
                ies.elseblock->accept(*this);
            }
            AFTER_VISIT_MACRO(ies);
        }
        void Runner::visit(syntax::ast::ForStatement &fors) {
            BEFORE_VISIT_MACRO(fors);
            fors.loop_exp->accept(*this);
            sp<Array> arr = dynamic_cast<Array*>(cur_v);
            if (arr == nullptr) {
                throw ConvertException(fors.loop_exp);
            }
            sp<Environment> before = cur_e;
            for (sp<Value> v : arr->value) {
                sp<Environment> inner = memory->create<Environment>(memory,before);
                inner->define(fors.loop_var, v);
                cur_e = inner;
                fors.block->accept(*this);
                sp<Return> ret = dynamic_cast<Return*>(cur_v);
                if (ret != nullptr) {
                    break;
                }
            }
            cur_e = before;
            AFTER_VISIT_MACRO(fors);
        }
        void Runner::visit(syntax::ast::Assignment &assign) {
            BEFORE_VISIT_MACRO(assign);
            if(cur_e->has(assign.id)){
                auto p = dynamic_cast<Ref*>(cur_e->get(assign.id));
                if(p == nullptr){
                    throw ConvertException(&(assign));
                }
                assign.value->accept(*this);
                p->push_change(memory->create<timemachine::RefChange>(p->to,cur_v));
                p->to = cur_v;
                cur_v = p;
            }else{
                throw NoSuchVariableException(&(assign));
            }
            AFTER_VISIT_MACRO(assign);
        }

        // do not eval firsttime.
        void Runner::visit(syntax::ast::TestFlyLine &) {
            //BEFORE_VISIT_MACRO(line);
            return;
        }
        void Runner::visit(syntax::ast::IdleFlyLine &) {
            //BEFORE_VISIT_MACRO(line);
            return;
        }

        void Runner::visit(syntax::ast::FlyMark& mark) {
            BEFORE_VISIT_MACRO(mark);
            mark.left->accept(*this); // save runtimevalue
            AFTER_VISIT_MACRO(mark);
        }

        void Runner::visit(syntax::ast::SourceCode &sc) {
            BEFORE_VISIT_MACRO(sc);
            merge_lambda_marker_map(sc.marker_to_lambda);
            merge_where_is_function_from(sc.where_is_function_from);
            for (auto s : sc.statements) {
                s->accept(*this);
            }
            AFTER_VISIT_MACRO(sc);
        }
        void Runner::visit(syntax::ast::DSL::DataDSL& dsl){
            BEFORE_VISIT_MACRO(dsl);
            cur_v = DSL::run_dsl(memory, dsl.inner,marker_to_lambda,cur_e);
            AFTER_VISIT_MACRO(dsl);
        }
        void Runner::merge_lambda_marker_map(const std::map<syntax::ast::Identifier,
                                             sp<syntax::ast::Function> >& m){
            marker_to_lambda = merge_map(marker_to_lambda,m);
        }
        void Runner::merge_where_is_function_from(const std::map<sp<syntax::ast::Block>,
                                                  sp<syntax::ast::Function> >& m){
            where_is_function_from = merge_map(where_is_function_from,m);
        }
    }
}
