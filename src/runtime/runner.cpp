#include "runner.hpp"
#include <iostream>
#include <memory>

namespace shiranui{
    namespace runtime{
        using shiranui::runtime::value::Integer;
        using shiranui::runtime::value::String;
        using shiranui::runtime::value::Function;
        using shiranui::runtime::value::UserFunction;
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
            std::cerr << "no eval identifier" << std::endl;
            return;
        }
        void Runner::visit(syntax::ast::Variable& var){
            if(cur.e->has(var.value)){
                cur.set_value(cur.e->get(var.value));
                return;
            }else{
                std::cerr << "there is no such variable" << var << std::endl;
            }
            return;
        }
        void Runner::visit(syntax::ast::Number& num){
            cur.set_value(std::make_shared<Integer>(num.value));
        }
        void Runner::visit(syntax::ast::String& s){
            cur.set_value(std::make_shared<String>(s.value));
        }
        void Runner::visit(syntax::ast::Block& block){
            Runner br(this);
            for(auto& st : block.statements){
                st->accept(br);
                cur.v = br.cur.v;
                // check return
            }
        }
        void Runner::visit(syntax::ast::Function& f){
            cur.set_value(std::make_shared<UserFunction>(f.parameters,f.body));
        }
        void Runner::visit(syntax::ast::FunctionCall& fc){
//            fc.function->accept(*this);
//            sp<Value> func = cur.v;
//            // check func.v is really function.
//            sp<Function> f = std::dynamic_pointer_cast<Function>(func);
//            if(f == NULL){
//                std::cerr << "It is not function" << std::endl;
//                return;
//            }
//            f->apply(*this,fc.arguments);
//            return;
        }
        void Runner::visit(syntax::ast::BinaryOperator&){
            return;
        }
        void Runner::visit(syntax::ast::UnaryOperator&){
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
        void Runner::visit(syntax::ast::IfElseStatement&){
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
