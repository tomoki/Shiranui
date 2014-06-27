#include "runner.hpp"
#include <iostream>
#include <typeinfo>
#include <memory>
#include <boost/thread/thread.hpp>

namespace shiranui{
    namespace runtime{
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
        using shiranui::runtime::value::builtin::PrintFunction;
        using shiranui::runtime::value::builtin::LengthFunction;
        Runner::Runner() :
            cur_v(std::make_shared<Integer>(0)),
            cur_e(std::make_shared<Environment>())
        {}
        void Runner::visit(syntax::ast::Identifier& id){
            throw RuntimeException(); // never occur.
            return;
        }
        void Runner::visit(syntax::ast::Variable& var){
            boost::this_thread::interruption_point();
            if(var.value.name == "system_call"){
                cur_v = std::make_shared<SystemCall>();
                return;
            }
            if(cur_e->has(var.value)){
                cur_v = cur_e->get(var.value);
                return;
            }else{
                // is it really ok?
                throw NoSuchVariableException(std::make_shared<syntax::ast::Variable>(var));
            }
        }
        void Runner::visit(syntax::ast::Number& num){
            boost::this_thread::interruption_point();
            cur_v = std::make_shared<Integer>(num.value);
        }
        void Runner::visit(syntax::ast::String& s){
            boost::this_thread::interruption_point();
            cur_v = std::make_shared<String>(s.value);
        }
        void Runner::visit(syntax::ast::Enum& e){
            boost::this_thread::interruption_point();
            std::vector<sp<Value>> vs;
            for(sp<syntax::ast::Expression> exp : e.expressions){
                exp->accept(*this);
                vs.push_back(cur_v);
            }
            cur_v = std::make_shared<Array>(vs);
        }
        void Runner::visit(syntax::ast::Interval& intr){
            boost::this_thread::interruption_point();
            sp<Value> start,end,next;
            {
                intr.start->accept(*this);
                start = cur_v;
                intr.end->accept(*this);
                end = cur_v;
                if(intr.next != nullptr){
                    intr.next->accept(*this);
                    next = cur_v;
                }
            }
            {
                sp<Integer> s = std::dynamic_pointer_cast<Integer>(start);
                sp<Integer> e = std::dynamic_pointer_cast<Integer>(end);
                if(s != nullptr and e != nullptr){
                    int change = 0;
                    if(next != nullptr){
                        sp<Integer> n = std::dynamic_pointer_cast<Integer>(next);
                        if(n == nullptr){
                            throw ConvertException(std::make_shared<syntax::ast::Interval>(intr));
                        }
                        change = n->value - s->value;
                        if(change == 0){
                            throw RangeException(std::make_shared<syntax::ast::Interval>(intr));
                        }
                    }
                    std::vector<sp<Value>> vs;
                    if(s->value <= e->value){
                        // if next == nullptr
                        if(change == 0) change = 1;
                        if(intr.right_close){
                            for(int i=s->value;i<=e->value;i+=change){
                                boost::this_thread::interruption_point();
                                vs.push_back(std::make_shared<Integer>(i));
                            }
                        }else{
                            for(int i=s->value;i<e->value;i+=change){
                                boost::this_thread::interruption_point();
                                vs.push_back(std::make_shared<Integer>(i));
                            }
                        }
                    }else{
                        // if next == nullptr
                        if(change == 0) change = -1;
                        if(intr.right_close){
                            for(int i=s->value;i>=e->value;i+=change){
                                boost::this_thread::interruption_point();
                                vs.push_back(std::make_shared<Integer>(i));
                            }
                        }else{
                            for(int i=s->value;i>e->value;i+=change){
                                boost::this_thread::interruption_point();
                                vs.push_back(std::make_shared<Integer>(i));
                            }
                        }
                    }
                    cur_v = std::make_shared<Array>(vs);
                    return;
                }
            }
            throw ConvertException(std::make_shared<syntax::ast::Interval>(intr));
        }

        void Runner::visit(syntax::ast::Block& block){
            boost::this_thread::interruption_point();
            sp<Environment> before = cur_e;
            sp<Environment> inner = std::make_shared<Environment>(cur_e);
            cur_e = inner;
            for(auto& st : block.statements){
                st->accept(*this);
                sp<Return> r = std::dynamic_pointer_cast<Return>(cur_v);
                if(r != nullptr){
                    break;
                }
            }
            cur_e = before;
        }
        void Runner::visit(syntax::ast::Function& f){
            boost::this_thread::interruption_point();
            cur_v = std::make_shared<UserFunction>(f.parameters,f.body,cur_e);
        }
        void Runner::visit(syntax::ast::FunctionCall& fc){
            boost::this_thread::interruption_point();
            fc.function->accept(*this);
            sp<Value> func = cur_v;
            // check func.v is really function.
            {
                sp<UserFunction> f = std::dynamic_pointer_cast<UserFunction>(func);
                if(f != nullptr){
                    if(fc.arguments.size() != f->parameters.size()){
                        throw ConvertException(std::make_shared<syntax::ast::FunctionCall>(fc));
                    }
                    sp<Environment> before = this->cur_e;
                    sp<Environment> call_env = std::make_shared<Environment>(f->env);
                    for(int i=0;i<f->parameters.size();i++){
                        fc.arguments[i]->accept(*this);
                        // it is not const.
                        call_env->define(f->parameters[i],cur_v,false);
                    }
                    this->cur_e = call_env;
                    f->body->accept(*this);
                    this->cur_e = before;
                    sp<Return> ret = std::dynamic_pointer_cast<Return>(this->cur_v);
                    if(ret == nullptr){
                        std::cerr << "WARN: this is not return value." << std::endl;
                    }else{
                        cur_v = ret->value;
                    }
                    return;
                }
            }
            {
                sp<SystemCall> f = std::dynamic_pointer_cast<SystemCall>(func);
                if(f != nullptr){
                    if(fc.arguments.size() != 1){
                        throw ConvertException(std::make_shared<syntax::ast::FunctionCall>(fc));
                    }
                    fc.arguments[0]->accept(*this);
                    sp<String> s = std::dynamic_pointer_cast<String>(cur_v);
                    if(s == nullptr){
                        throw ConvertException(std::make_shared<syntax::ast::FunctionCall>(fc));
                    }else{
                        if(s->value == "print"){
                            cur_v = std::make_shared<PrintFunction>();
                        }else if(s->value == "length"){
                            cur_v = std::make_shared<LengthFunction>();
                        }else if(s->value == "len"){
                        }else{
                            throw ConvertException(std::make_shared<syntax::ast::FunctionCall>(fc));
                        }
                    }
                    return;
                }
            }
            {
                sp<BuiltinFunction> f = std::dynamic_pointer_cast<BuiltinFunction>(func);
                if(f != nullptr){
                    {
                        std::vector<sp<Value>> arguments;
                        for(sp<syntax::ast::Expression> arg : fc.arguments){
                            arg->accept(*this);
                            arguments.push_back(cur_v);
                        }
                        sp<Value> ret = f->run(arguments);
                        if(ret == nullptr){
                            throw ConvertException(std::make_shared<syntax::ast::FunctionCall>(fc));
                        }else{
                            cur_v = ret;
                        }
                        return;
                    }
                }
            }
            throw ConvertException(std::make_shared<syntax::ast::FunctionCall>(fc));
        }
        void Runner::visit(syntax::ast::BinaryOperator& bop){
            boost::this_thread::interruption_point();
            bop.left->accept(*this);
            sp<Value> left = cur_v;
            bop.right->accept(*this);
            sp<Value> right = cur_v;
            if(typeid(*left) != typeid(*right)){
                throw ConvertException(std::make_shared<syntax::ast::BinaryOperator>(bop));
            }
            {
                sp<Integer> l = std::dynamic_pointer_cast<Integer>(left);
                sp<Integer> r = std::dynamic_pointer_cast<Integer>(right);
                if(l != nullptr and r != nullptr){
                    if(bop.op == "="){
                        cur_v = std::make_shared<Boolean>(l->value == r->value);
                    }else if(bop.op == "/="){
                        cur_v = std::make_shared<Boolean>(l->value != r->value);
                    }else if(bop.op == "<"){
                        cur_v = std::make_shared<Boolean>(l->value < r->value);
                    }else if(bop.op == "<="){
                        cur_v = std::make_shared<Boolean>(l->value <= r->value);
                    }else if(bop.op == ">"){
                        cur_v = std::make_shared<Boolean>(l->value > r->value);
                    }else if(bop.op == ">="){
                        cur_v = std::make_shared<Boolean>(l->value >= r->value);
                    }else if(bop.op == "+"){
                        cur_v = std::make_shared<Integer>(l->value+r->value);
                    }else if(bop.op == "-"){
                        cur_v = std::make_shared<Integer>(l->value-r->value);
                    }else if(bop.op == "*"){
                        cur_v = std::make_shared<Integer>(l->value*r->value);
                    }else if(bop.op == "/"){
                        cur_v = std::make_shared<Integer>(l->value/r->value);
                    }else if(bop.op == "%"){
                        cur_v = std::make_shared<Integer>(l->value%r->value);
                    }else if(bop.op == "^"){
                    }else{
                        throw ConvertException(std::make_shared<syntax::ast::BinaryOperator>(bop));
                    }
                    return;
                }
            }
            {
                sp<Boolean> l = std::dynamic_pointer_cast<Boolean>(left);
                sp<Boolean> r = std::dynamic_pointer_cast<Boolean>(right);
                if(l != nullptr and r != nullptr){
                    if(bop.op == "="){
                        cur_v = std::make_shared<Boolean>(l->value == r->value);
                    }else if(bop.op == "/="){
                        cur_v = std::make_shared<Boolean>(l->value != r->value);
                    }else if(bop.op == "and"){
                        cur_v = std::make_shared<Boolean>(l->value and r->value);
                    }else if(bop.op == "or"){
                        cur_v = std::make_shared<Boolean>(l->value or r->value);
                    }else{
                        throw ConvertException(std::make_shared<syntax::ast::BinaryOperator>(bop));
                    }
                    return;
                }
            }
            {
                sp<String> l = std::dynamic_pointer_cast<String>(left);
                sp<String> r = std::dynamic_pointer_cast<String>(right);
                if(l != nullptr and r != nullptr){
                    if(bop.op == "="){
                        cur_v = std::make_shared<Boolean>(l->value == r->value);
                    }else if(bop.op == "/="){
                        cur_v = std::make_shared<Boolean>(l->value != r->value);
                    }else if(bop.op == "+"){
                        cur_v = std::make_shared<String>(l->value+r->value);
                    }else{
                        throw ConvertException(std::make_shared<syntax::ast::BinaryOperator>(bop));
                    }
                    return;
                }

            }
            throw ConvertException(std::make_shared<syntax::ast::BinaryOperator>(bop));
            if(bop.op == "="){
            }else if(bop.op == "/="){
            }else if(bop.op == "+"){
            }else if(bop.op == "-"){
            }else if(bop.op == "*"){
            }else if(bop.op == "/"){
            }else if(bop.op == "%"){
            }else if(bop.op == "^"){
            }else if(bop.op == "and"){
            }else if(bop.op == "or"){
            }

        }
        void Runner::visit(syntax::ast::UnaryOperator& uop){
            boost::this_thread::interruption_point();
            uop.exp->accept(*this);
            sp<Value> v_ = cur_v;
            {
                sp<Integer> v = std::dynamic_pointer_cast<Integer>(v_);
                if(v != nullptr){
                    if(uop.op == "+"){
                        cur_v = v;
                        return;
                    }else if(uop.op == "-"){
                        cur_v = std::make_shared<Integer>(-(v->value));
                        return;
                    }
                }
            }
            {
                sp<Boolean> v = std::dynamic_pointer_cast<Boolean>(v_);
                if(v != nullptr){
                    if(uop.op == "not"){
                        cur_v = std::make_shared<Boolean>(not (v->value));
                        return;
                    }
                }
            }

            throw ConvertException(std::make_shared<syntax::ast::UnaryOperator>(uop));
        }
        void Runner::visit(syntax::ast::IfElseExpression&){
            boost::this_thread::interruption_point();
            return;
        }
        void Runner::visit(syntax::ast::Definement& def){
            boost::this_thread::interruption_point();
            def.value->accept(*this);
            cur_e->define(def.id,cur_v,def.is_const);
            return;
        }
        void Runner::visit(syntax::ast::ReturnStatement& ret){
            boost::this_thread::interruption_point();
            ret.value->accept(*this);
            cur_v = std::make_shared<Return>(cur_v);
            return;
        }
        void Runner::visit(syntax::ast::IfElseStatement& ies){
            boost::this_thread::interruption_point();
            ies.pred->accept(*this);
            sp<Boolean> bp = std::dynamic_pointer_cast<Boolean>(cur_v);
            if(bp == nullptr){
                throw ConvertException(ies.pred);
            }
            if(bp->value){
                ies.ifblock->accept(*this);
            }else{
                ies.elseblock->accept(*this);
            }
            return;
        }
        void Runner::visit(syntax::ast::ForStatement& fors){
            boost::this_thread::interruption_point();
            fors.loop_exp->accept(*this);
            sp<Array> arr = std::dynamic_pointer_cast<Array>(cur_v);
            if(arr == nullptr){
                throw ConvertException(fors.loop_exp);
            }
            sp<Environment> before = cur_e;
            for(sp<Value> v : arr->value){
                sp<Environment> inner = std::make_shared<Environment>(before);
                inner->define(fors.loop_var,v,false);
                cur_e = inner;
                fors.block->accept(*this);
                sp<Return> ret = std::dynamic_pointer_cast<Return>(cur_v);
                if(ret != nullptr){
                    break;
                }
            }
            cur_e = before;
            return;
        }
        void Runner::visit(syntax::ast::Assignment& assign){
            boost::this_thread::interruption_point();
            if(cur_e->has(assign.id) and
               not cur_e->is_const(assign.id)){
                assign.value->accept(*this);
                cur_e->set(assign.id,cur_v);
            }
            return;
        }

        // do not eval firsttime.
        void Runner::visit(syntax::ast::TestFlyLine& line){
            boost::this_thread::interruption_point();
            return;
        }
        void Runner::visit(syntax::ast::IdleFlyLine& line){
            boost::this_thread::interruption_point();
            return;
        }

        void Runner::visit(syntax::ast::SourceCode& sc){
            boost::this_thread::interruption_point();
            for(auto s : sc.statements){
                s->accept(*this);
            }
            return;
        }
    }
}
