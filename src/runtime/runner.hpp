#ifndef RUNNER_HPP_INCLUDED
#define RUNNER_HPP_INCLUDED

#include "value.hpp"
#include "environment.hpp"

namespace shiranui{
    namespace runtime{
        using shiranui::environment::Environment;
        using shiranui::runtime::value::Value;
        struct ValEnv{
            sp<Value> v;
            Environment e;
            sp<ValEnv> set_value(Value* val){
                sp<ValEnv> nve = sp<ValEnv>(new ValEnv());
                nve->e = e;
                nve->v = sp<Value>(val);
                return nve;
            }
            sp<ValEnv> set_value(sp<Value> val){
                sp<ValEnv> nve = sp<ValEnv>(new ValEnv());
                nve->e = e;
                nve->v = val;
                return nve;
            }
            sp<ValEnv> backup(){
                sp<ValEnv> nve = sp<ValEnv>(new ValEnv());
                nve->e = e;
                nve->v = v;
                return nve;
            }

        };
        struct Runner : VisitorForAST<sp<ValEnv>>{
            sp<ValEnv> prev;
            Runner(sp<ValEnv> e);
            Runner(ValEnv* e);
            Runner();
            sp<ValEnv> visit(syntax::ast::Identifier&);
            sp<ValEnv> visit(syntax::ast::Variable&);
            sp<ValEnv> visit(syntax::ast::Number&);
            sp<ValEnv> visit(syntax::ast::String&);
            sp<ValEnv> visit(syntax::ast::Block&);
            sp<ValEnv> visit(syntax::ast::Function&);
            sp<ValEnv> visit(syntax::ast::FunctionCall&);
            sp<ValEnv> visit(syntax::ast::BinaryOperator&);
            sp<ValEnv> visit(syntax::ast::UnaryOperator&);
            sp<ValEnv> visit(syntax::ast::IfElseExpression&);
            sp<ValEnv> visit(syntax::ast::Definement&);
            sp<ValEnv> visit(syntax::ast::ReturnStatement&);
            sp<ValEnv> visit(syntax::ast::IfElseStatement&);
            sp<ValEnv> visit(syntax::ast::SourceCode&);
        };
    }
}
#endif
