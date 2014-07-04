#ifndef RUNNER_HPP_INCLUDED
#define RUNNER_HPP_INCLUDED

#include "value.hpp"
#include "environment.hpp"
#include <exception>
#include <stack>

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
        struct InternalException : RuntimeException{
            InternalException(sp<syntax::ast::LocationInfo> e){
                where = e;
            }
        };
        struct Runner : shiranui::syntax::ast::VisitorForAST{
            sp<value::Value> cur_v;
            sp<environment::Environment> cur_e;
            int cur_t;
            std::stack<int> call_stack;
            bool is_server;
            Runner(bool=false);
            template<typename T>
            int before_visit(T&);
            template<typename T>
            void after_visit(T&,int);

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
            void visit(syntax::ast::TestFlyLine&);
            void visit(syntax::ast::IdleFlyLine&);
            void visit(syntax::ast::SourceCode&);
        };
    }
}
#endif
