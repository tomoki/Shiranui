#ifndef DIVER_HPP_INCLUDED
#define DIVER_HPP_INCLUDED

#include "../../syntax/ast.hpp"
#include <string>
#include <stack>
#include <utility>

namespace shiranui{
    namespace runtime{
        namespace diver{
            const std::string STRIKE = "strike";
            const std::string EXPLORE = "explore";

            struct DivingMessage{
                std::string cache;
                std::string str();
                template<typename T>
                DivingMessage add_strike(const T&);
                template<typename T>
                DivingMessage add_explore(const T&,const std::string&);
                DivingMessage operator+(DivingMessage);
            };
            template<typename T>
            sp<syntax::ast::LocationInfo> use_swimfin(T&,int);
            template<typename T>
            DivingMessage use_snorkel(T&,int);

            struct Diver{
                sp<syntax::ast::SourceCode> source;
                int current_id;
                std::stack<std::pair<int,sp<syntax::ast::Expression>>> undo_stack;
                Diver(sp<syntax::ast::SourceCode>);

                DivingMessage clear();
                DivingMessage dive(int);
                DivingMessage dive(sp<syntax::ast::Expression>);
                DivingMessage dive(syntax::ast::FunctionCall&);

                DivingMessage dive(sp<syntax::ast::Expression>,int);
                DivingMessage dive(syntax::ast::FunctionCall&,int);
                DivingMessage see(syntax::ast::Block&,int);
                DivingMessage surface(); // undo
            };
            struct SwimFin : syntax::ast::VisitorForAST{
                sp<syntax::ast::Expression> treasure;
                int point;
                SwimFin(int);
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

                template<typename T>
                bool in_range(T&);
                template<typename T>
                bool in_range(sp<T>);
            };
            struct Snorkel : syntax::ast::VisitorForAST{
                int call_under;
                DivingMessage message;
                Snorkel(int);
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
}
#endif
