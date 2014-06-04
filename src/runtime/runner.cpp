#include "runner.hpp"
#include <iostream>

namespace shiranui{
    namespace runtime{
        using shiranui::runtime::value::Integer;
        using shiranui::runtime::value::String;
        using shiranui::runtime::value::Function;
        Runner::Runner(ValEnv* e) : prev(e) {}
        Runner::Runner(sp<ValEnv> e) : prev(e) {}
        Runner::Runner() : prev(new ValEnv()) {}
        sp<ValEnv> Runner::visit(syntax::ast::Identifier& id){
            std::cerr << "no eval identifier" << std::endl;
            return prev;
        }
        sp<ValEnv> Runner::visit(syntax::ast::Variable& var){
            if(prev->e.has(var)){
                return prev->set_value(prev->e.get(var));
            }
            std::cerr << "there is no such variable" << var << std::endl;
            return prev;
        }
        sp<ValEnv> Runner::visit(syntax::ast::Number& num){
            return prev->set_value(new Integer(num.value));
        }
        sp<ValEnv> Runner::visit(syntax::ast::String& s){
            return prev->set_value(new String(s.value));
        }
        sp<ValEnv> Runner::visit(syntax::ast::Block& block){
            for(auto& st : block.statements){
                prev = st->accept(*this);
            }
            // do scope.
            return prev;
        }
        sp<ValEnv> Runner::visit(syntax::ast::Function& f){
            return prev->set_value(new Function(f.parameters,f.body));
        }
        sp<ValEnv> Runner::visit(syntax::ast::FunctionCall& fc){
            sp<ValEnv> before_call = prev->backup();;
            sp<ValEnv> func = fc.function->accept(*this);
            // check func.v is really function.
            //  rewrite.
            sp<Function> f = std::dynamic_pointer_cast<Function>(func->v);
            if(f == NULL){
                std::cerr << "it is not function" << std::endl;
                return before_call;
            }
            // TODO:complete it.
            std::vector<sp<Value>> arguments;
            for(auto arg : fc.arguments){
                sp<ValEnv> a = arg->accept(*this);
                arguments.push_back(a->v);
            }
            if(arguments.size() != f->parameters.size()){
                std::cerr << "arguments mismatch" << std::endl;
                // TODO tekitou
                return prev;
            }else{
                for(int i=0;i<arguments.size();i++){
                    prev->e.set(f->parameters[i],arguments[i]);
                }
                // use new runner.
                sp<ValEnv> after_call = f->body->accept(*this);
                return before_call->set_value(after_call->v);
            }
        }
        sp<ValEnv> Runner::visit(syntax::ast::BinaryOperator&){
            return prev;
        }
        sp<ValEnv> Runner::visit(syntax::ast::UnaryOperator&){
            return prev;
        }
        sp<ValEnv> Runner::visit(syntax::ast::IfElseExpression&){
            return prev;
        }
        sp<ValEnv> Runner::visit(syntax::ast::Definement& def){
            sp<ValEnv> s = def.value->accept(*this);
            prev->e.set(def.id,s->v);
            return prev;
        }
        sp<ValEnv> Runner::visit(syntax::ast::ReturnStatement&){
            return prev;
        }
        sp<ValEnv> Runner::visit(syntax::ast::IfElseStatement&){
            return prev;
        }
        sp<ValEnv> Runner::visit(syntax::ast::SourceCode& sc){
            for(auto s : sc.statements){
                prev = s->accept(*this);
            }
            return prev;
        }

    }
}
