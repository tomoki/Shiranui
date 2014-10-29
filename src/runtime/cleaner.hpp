#ifndef CLEANER_HPP_INCLUDED
#define CLEANER_HPP_INCLUDED

#include "../syntax/ast.hpp"
#include "runtime_info.hpp"

namespace shiranui{
    namespace runtime{
        namespace infomation{
            // Cleaner remove all runtime_info from tree.
            struct Cleaner : syntax::ast::VisitorForAST{
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
                void visit(syntax::ast::ExpressionStatement&);
                void visit(syntax::ast::ReturnStatement&);
                void visit(syntax::ast::AssertStatement&);
                void visit(syntax::ast::ProbeStatement&);
                void visit(syntax::ast::IfElseStatement&);
                void visit(syntax::ast::ForStatement&);
                void visit(syntax::ast::Assignment&);
                void visit(syntax::ast::TestFlyLine&);
                void visit(syntax::ast::IdleFlyLine&);
                void visit(syntax::ast::FlyMark&);
                void visit(syntax::ast::SourceCode&);
                template<typename T>
                void clear_it(T& t);
            };
            struct ValueCleaner : runtime::value::VisitorForValue{
                void visit(runtime::value::Integer&);
                void visit(runtime::value::String&);
                void visit(runtime::value::Boolean&);
                void visit(runtime::value::Array&);
                void visit(runtime::value::UserFunction&);
                void visit(runtime::value::Return&);
                void visit(runtime::value::SystemCall&);
                void visit(runtime::value::BuiltinFunction&);
            };
        }
    }
}
#endif
