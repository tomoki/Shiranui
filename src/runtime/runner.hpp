#ifndef RUNNER_HPP_INCLUDED
#define RUNNER_HPP_INCLUDED

#include "value.hpp"
#include "environment.hpp"
#include <exception>

namespace shiranui{
    namespace runtime{
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
        struct RangeException : RuntimeException{
            RangeException(sp<syntax::ast::LocationInfo> e){
                where = e;
            }
        };
        struct ValEnv{
            sp<value::Value> v;
            sp<environment::Environment> e;
            ValEnv();
            ValEnv(environment::Environment*);
            ValEnv(sp<environment::Environment>);
            void set_value(sp<value::Value>);
        };
        struct Runner : shiranui::syntax::ast::VisitorForAST{
            ValEnv cur;
            Runner();
            Runner(Runner*); // for block
            void visit(syntax::ast::Identifier&);
            void visit(syntax::ast::Variable&);
            void visit(syntax::ast::Number&);
            void visit(syntax::ast::String&);
            void visit(syntax::ast::Enum&);
            void visit(syntax::ast::Interval&);
            void visit(syntax::ast::Block&);
            void visit(syntax::ast::Function&);
            void visit(syntax::ast::FunctionCall&);
            void visit(syntax::ast::BinaryOperator&);
            void visit(syntax::ast::UnaryOperator&);
            void visit(syntax::ast::IfElseExpression&);
            void visit(syntax::ast::Definement&);
            void visit(syntax::ast::ReturnStatement&);
            void visit(syntax::ast::IfElseStatement&);
            void visit(syntax::ast::ForStatement&);
            void visit(syntax::ast::Assignment&);
            void visit(syntax::ast::FlyLine&);
            void visit(syntax::ast::SourceCode&);
        };
    }
}
#endif
