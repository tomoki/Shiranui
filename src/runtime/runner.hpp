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
            ValEnv set_value(Value* val){
                return set_value(sp<Value>(val));
            }
            ValEnv set_value(sp<Value> val){
                ValEnv nve = *this;
                nve.v = val;
                return nve;
            }

        };
        struct Runner : VisitorForAST{
            ValEnv prev;
            Runner();
            void visit(syntax::ast::Identifier&);
            void visit(syntax::ast::Variable&);
            void visit(syntax::ast::Number&);
            void visit(syntax::ast::String&);
            void visit(syntax::ast::Block&);
            void visit(syntax::ast::Function&);
            void visit(syntax::ast::FunctionCall&);
            void visit(syntax::ast::BinaryOperator&);
            void visit(syntax::ast::UnaryOperator&);
            void visit(syntax::ast::IfElseExpression&);
            void visit(syntax::ast::Definement&);
            void visit(syntax::ast::ReturnStatement&);
            void visit(syntax::ast::IfElseStatement&);
            void visit(syntax::ast::SourceCode&);
        };
    }
}
#endif
