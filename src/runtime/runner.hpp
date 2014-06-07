#ifndef RUNNER_HPP_INCLUDED
#define RUNNER_HPP_INCLUDED

#include "value.hpp"
#include "environment.hpp"
#include <exception>

namespace shiranui{
    namespace runtime{
        using shiranui::runtime::environment::Environment;
        using shiranui::runtime::value::Value;
        struct RuntimeException : std::exception{
            sp<syntax::ast::LocationInfo> where;
        };
        struct NoSuchVariableException : RuntimeException{
            NoSuchVariableException(sp<syntax::ast::LocationInfo> e){
                where = e;
            }
        };
        struct ConvertException : RuntimeException{
            ConvertException(sp<syntax::ast::LocationInfo> e){
                where = e;
            }
        };
        struct ValEnv{
            sp<Value> v;
            sp<Environment> e;
            ValEnv();
            ValEnv(Environment*);
            ValEnv(sp<Environment>);
            void set_value(sp<Value>);
        };
        struct Runner : shiranui::syntax::ast::VisitorForAST{
            ValEnv cur;
            Runner();
            Runner(Runner*); // for block
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
