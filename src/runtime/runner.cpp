#include "runner.hpp"
#include <iostream>
#include <memory>

namespace shiranui{
    namespace runtime{
        using shiranui::runtime::value::Integer;
        using shiranui::runtime::value::String;
        using shiranui::runtime::value::Function;
        Runner::Runner() {}
        void Runner::visit(syntax::ast::Identifier& id){
            std::cerr << "no eval identifier" << std::endl;
            return;
        }
        void Runner::visit(syntax::ast::Variable& var){
            if(prev.e.has(var)){
                prev = prev.set_value(prev.e.get(var));
                return;
            }else{
                std::cerr << "there is no such variable" << var << std::endl;
            }
            return;
        }
        void Runner::visit(syntax::ast::Number& num){
            prev = prev.set_value(std::make_shared<Integer>(num.value));
        }
        void Runner::visit(syntax::ast::String& s){
            prev = prev.set_value(std::make_shared<String>(s.value));
        }
        void Runner::visit(syntax::ast::Block& block){
            // TODO: do scoping
            for(auto& st : block.statements){
                st->accept(*this);
            }
        }
        void Runner::visit(syntax::ast::Function& f){
            prev = prev.set_value(std::make_shared<Function>(f.parameters,f.body));
        }
        void Runner::visit(syntax::ast::FunctionCall& fc){
            ValEnv before_call = prev;
            fc.function->accept(*this);
            sp<Value> func = prev.v;
            // check func.v is really function.
            //  rewrite.
            sp<Function> f = std::dynamic_pointer_cast<Function>(func);
            if(f == NULL){
                std::cerr << "It is not function" << std::endl;
                return;
            }
            // TODO:complete it.
            std::vector<sp<Value>> arguments;
            for(auto arg : fc.arguments){
                arg->accept(*this);
                arguments.push_back(prev.v);
            }
            if(arguments.size() != f->parameters.size()){
                std::cerr << "arguments mismatch" << std::endl;
                return;
            }else{
                for(int i=0;i<arguments.size();i++){
                    prev.e.set(f->parameters[i],arguments[i]);
                }
                // use new runner.
                f->body->accept(*this);
                prev = before_call.set_value(prev.v);
            }
            return;
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
            prev.e.set(def.id,prev.v);
            return;
        }
        void Runner::visit(syntax::ast::ReturnStatement&){
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
