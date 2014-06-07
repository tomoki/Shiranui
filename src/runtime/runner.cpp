#include "runner.hpp"
#include <iostream>
#include <typeinfo>
#include <memory>

namespace shiranui{
    namespace runtime{
        using shiranui::runtime::value::Integer;
        using shiranui::runtime::value::String;
        using shiranui::runtime::value::Boolean;
        using shiranui::runtime::value::Function;
        using shiranui::runtime::value::UserFunction;
//        using shiranui::runtime::value::BuiltinFunction;
        using shiranui::runtime::value::Return;
        ValEnv::ValEnv(){
            v = std::make_shared<Integer>(0);
            e = std::make_shared<Environment>();
        }
        ValEnv::ValEnv(Environment* e_){
            v = std::make_shared<Integer>(0);
            e = std::make_shared<Environment>(e_);
        }
        ValEnv::ValEnv(sp<Environment> e_){
            v = std::make_shared<Integer>(0);
            e = std::make_shared<Environment>(e_);
        }
        void ValEnv::set_value(sp<Value> v_){
            v = v_;
        }
        Runner::Runner() {}
        Runner::Runner(Runner* outer)
            : cur(outer->cur.e){
        }
        void Runner::visit(syntax::ast::Identifier& id){
            throw RuntimeException(); // never occur.
            return;
        }
        void Runner::visit(syntax::ast::Variable& var){
            if(cur.e->has(var.value)){
                cur.v = cur.e->get(var.value);
                return;
            }else{
                // is it really ok?
                throw NoSuchVariableException(sp<syntax::ast::Variable>(&var));
            }
        }
        void Runner::visit(syntax::ast::Number& num){
            cur.v = std::make_shared<Integer>(num.value);
        }
        void Runner::visit(syntax::ast::String& s){
            cur.v = std::make_shared<String>(s.value);
        }
        void Runner::visit(syntax::ast::Block& block){
            Runner br(this);
            for(auto& st : block.statements){
                st->accept(br);
                cur.v = br.cur.v;
                sp<Return> r = std::dynamic_pointer_cast<Return>(cur.v);
                if(r != nullptr){
                    return;
                }
            }
        }
        void Runner::visit(syntax::ast::Function& f){
            cur.set_value(std::make_shared<UserFunction>(f.parameters,f.body));
        }
        void Runner::visit(syntax::ast::FunctionCall& fc){
            fc.function->accept(*this);
            sp<Value> func = cur.v;
            // check func.v is really function.
            {
                sp<UserFunction> f = std::dynamic_pointer_cast<UserFunction>(func);
                if(f != nullptr){
                    if(fc.arguments.size() != f->parameters.size()){
                        throw ConvertException(sp<syntax::ast::FunctionCall>(&fc));
                    }
                    Runner inner(this);
                    for(int i=0;i<f->parameters.size();i++){
                        fc.arguments[i]->accept(*this);
                        // it is not const.
                        inner.cur.e->define(f->parameters[i],cur.v,false);
                    }
                    f->body->accept(inner);
                    sp<Return> ret = std::dynamic_pointer_cast<Return>(inner.cur.v);
                    if(ret == nullptr){
                        std::cerr << "WARN: this is not return value." << std::endl;
                    }else{
                        cur.v = ret->value;
                    }
                    return;
                }
            }
//            f = std::dynamic_pointer_cast<BuiltinFunction>(func);
//             if(f != nullptr){
//                 return;
//             }
            throw ConvertException(sp<syntax::ast::FunctionCall>(&fc));
        }
        void Runner::visit(syntax::ast::BinaryOperator& bop){
            bop.left->accept(*this);
            sp<Value> left = cur.v;
            bop.right->accept(*this);
            sp<Value> right = cur.v;
            if(typeid(*left) != typeid(*right)){
                throw ConvertException(sp<syntax::ast::BinaryOperator>(&bop));
            }
            {
                sp<Integer> l = std::dynamic_pointer_cast<Integer>(left);
                sp<Integer> r = std::dynamic_pointer_cast<Integer>(right);
                if(l != nullptr and r != nullptr){
                    if(bop.op == "="){
                        cur.v = std::make_shared<Boolean>(l->value == r->value);
                    }else if(bop.op == "/="){
                        cur.v = std::make_shared<Boolean>(l->value != r->value);
                    }else if(bop.op == "+"){
                        cur.v = std::make_shared<Integer>(l->value+r->value);
                    }else if(bop.op == "-"){
                        cur.v = std::make_shared<Integer>(l->value-r->value);
                    }else if(bop.op == "*"){
                        cur.v = std::make_shared<Integer>(l->value*r->value);
                    }else if(bop.op == "/"){
                        cur.v = std::make_shared<Integer>(l->value/r->value);
                    }else if(bop.op == "%"){
                        cur.v = std::make_shared<Integer>(l->value%r->value);
                    }else if(bop.op == "^"){
                    }else{
                        throw ConvertException(sp<syntax::ast::BinaryOperator>(&bop));
                    }
                    return;
                }
            }
            {
                sp<Boolean> l = std::dynamic_pointer_cast<Boolean>(left);
                sp<Boolean> r = std::dynamic_pointer_cast<Boolean>(right);
                if(l != nullptr and r != nullptr){
                    if(bop.op == "="){
                        cur.v = std::make_shared<Boolean>(l->value == r->value);
                    }else if(bop.op == "/="){
                        cur.v = std::make_shared<Boolean>(l->value != r->value);
                    }else if(bop.op == "and"){
                        cur.v = std::make_shared<Boolean>(l->value and r->value);
                    }else if(bop.op == "or"){
                        cur.v = std::make_shared<Boolean>(l->value or r->value);
                    }else{
                        throw ConvertException(sp<syntax::ast::BinaryOperator>(&bop));
                    }
                    return;
                }
            }
            throw ConvertException(sp<syntax::ast::BinaryOperator>(&bop));
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
                  if(uop.op == "not"){
            }else if(uop.op == "+"){
            }else if(uop.op == "-"){
            }
            return;
        }
        void Runner::visit(syntax::ast::IfElseExpression&){
            return;
        }
        void Runner::visit(syntax::ast::Definement& def){
            def.value->accept(*this);
            cur.e->define(def.id,cur.v,def.is_const);
            return;
        }
        void Runner::visit(syntax::ast::ReturnStatement& ret){
            ret.value->accept(*this);
            cur.v = std::make_shared<Return>(cur.v);
            return;
        }
        void Runner::visit(syntax::ast::IfElseStatement& ies){
            ies.pred->accept(*this);
            sp<Boolean> bp = std::dynamic_pointer_cast<Boolean>(cur.v);
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
        void Runner::visit(syntax::ast::SourceCode& sc){
            for(auto s : sc.statements){
                s->accept(*this);
            }
            return;
        }

    }
}
