#ifndef RUNNER_HPP_INCLUDED
#define RUNNER_HPP_INCLUDED

#include "value.hpp"
#include "environment.hpp"
#include <exception>
#include <sstream>
#include <stack>

namespace shiranui{
    namespace runtime{
        struct RuntimeException : std::exception{
            sp<syntax::ast::LocationInfo> where;
            std::string str(){
                return "\"Something occured.\"";
            }
        };
        struct NoSuchVariableException : RuntimeException{
            NoSuchVariableException(sp<syntax::ast::LocationInfo> e){
                where = e;
            }
            std::string str(){
                {
                    auto p = std::dynamic_pointer_cast<syntax::ast::Variable>(where);
                    if (p != nullptr) {
                        std::stringstream ss;
                        ss << '\"' << "No such variable: " << p->value.name << '\"';
                        return ss.str();
                    }
                }
                {
                    auto p = std::dynamic_pointer_cast<syntax::ast::Assignment>(where);
                    if (p != nullptr) {
                        std::stringstream ss;
                        ss << '\"' << "No such variable: " << p->id.name << '\"';
                        return ss.str();
                    }
                }
                return "\"No such variable: ????\"";
            }
        };
        struct ConvertException : RuntimeException{
            ConvertException(sp<syntax::ast::LocationInfo> e){
                where = e;
            }
            std::string str(){
                return "\"Can't convert something\"";
            }
        };
        struct RangeException : RuntimeException{
            RangeException(sp<syntax::ast::LocationInfo> e){
                where = e;
            }
            std::string str(){
                return "\"Invalid range\"";
            }
        };
        struct InternalException : RuntimeException{
            InternalException(sp<syntax::ast::LocationInfo> e){
                where = e;
            }
            std::string str(){
                return "\"Division by 0\"";
            }
        };
        struct ZeroDivException : RuntimeException{
            ZeroDivException(sp<syntax::ast::LocationInfo> e){
                where = e;
            }
            std::string str() {
                return "\"Max call-depth exceeded\"";
            }
        };
        struct AssertException : RuntimeException{
            AssertException(sp<syntax::ast::LocationInfo> e){
                where = e;
            }
            std::string str() {
                return "\"Assert violated\"";
            }
        };
        struct MaxDepthExceededException : RuntimeException{
            MaxDepthExceededException(sp<syntax::ast::LocationInfo> e){
                where = e;
            }
            std::string str(){
                return "\"Max call-depth exceeded\"";
            }
        };
        struct Runner : shiranui::syntax::ast::VisitorForAST{
            sp<value::Value> cur_v;
            sp<environment::Environment> cur_e;
            int cur_t;
            int call_depth;
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
            void visit(syntax::ast::ProbeStatement&);
            void visit(syntax::ast::AssertStatement&);
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
