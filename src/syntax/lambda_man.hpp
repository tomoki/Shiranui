#ifndef LAMBDAMAN_HPP_INCLUDED
#define LAMBDAMAN_HPP_INCLUDED

#include "../syntax/ast.hpp"
#include <map>
#include <set>
#include <stack>

namespace shiranui{
    namespace syntax{
        struct LambdaMarkerScanner : ast::VisitorForAST{
            std::map<ast::Block*,    ast::Function*> where_are_you_from;
            std::map<ast::Identifier,ast::Function*> marker_to_lambda;
            void visit(ast::Identifier&);
            void visit(ast::Variable&);
            void visit(ast::Number&);
            void visit(ast::String&);
            void visit(ast::Boolean&);
            void visit(ast::Enum&);
            void visit(ast::Interval&);
            void visit(ast::Block&);
            void visit(ast::Function&);
            void visit(ast::FunctionCall&);
            void visit(ast::BinaryOperator&);
            void visit(ast::UnaryOperator&);
            void visit(ast::IfElseExpression&);
            void visit(ast::Definement&);
            void visit(ast::ExpressionStatement&);
            void visit(ast::ReturnStatement&);
            void visit(ast::ProbeStatement&);
            void visit(ast::AssertStatement&);
            void visit(ast::IfElseStatement&);
            void visit(ast::ForStatement&);
            void visit(ast::Assignment&);
            void visit(ast::TestFlyLine&);
            void visit(ast::IdleFlyLine&);
            void visit(ast::FlyMark&);
            void visit(ast::SourceCode&);
            void visit(ast::DSL::DataDSL&);
        };
        std::pair<
            std::map<sp<ast::Block>,sp<ast::Function> >,
            std::map<ast::Identifier,sp<ast::Function> >
            >
        scan_lambda_marker(ast::SourceCode&);


        // ---------------------------------------------------------------------
        struct LambdaFreeVariableScanner : ast::VisitorForAST{
            std::set<ast::Identifier> free;
            std::stack<std::set<ast::Identifier> > bound;
            LambdaFreeVariableScanner();
            void visit(ast::Identifier&);
            void visit(ast::Variable&);
            void visit(ast::Number&);
            void visit(ast::String&);
            void visit(ast::Boolean&);
            void visit(ast::Enum&);
            void visit(ast::Interval&);
            void visit(ast::Block&);
            void visit(ast::Function&);
            void visit(ast::FunctionCall&);
            void visit(ast::BinaryOperator&);
            void visit(ast::UnaryOperator&);
            void visit(ast::IfElseExpression&);
            void visit(ast::Definement&);
            void visit(ast::ExpressionStatement&);
            void visit(ast::ReturnStatement&);
            void visit(ast::ProbeStatement&);
            void visit(ast::AssertStatement&);
            void visit(ast::IfElseStatement&);
            void visit(ast::ForStatement&);
            void visit(ast::Assignment&);
            void visit(ast::TestFlyLine&);
            void visit(ast::IdleFlyLine&);
            void visit(ast::FlyMark&);
            void visit(ast::SourceCode&);
            void visit(ast::DSL::DataDSL&);

            bool is_free(ast::Identifier);
        };
        std::set<ast::Identifier> scan_free_variable(ast::Function&);
        std::set<ast::Identifier> scan_free_variable(sp<ast::Function>);
    }
}
#endif
