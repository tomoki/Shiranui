#ifndef AST_HPP_INCLUDED
#define AST_HPP_INCLUDED

#include "../misc.hpp"
#include "../runtime/value.hpp"
#include <iostream>
#include <memory>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_fusion.hpp>
#include <boost/fusion/include/adapt_struct.hpp>

#include <string>
#include <vector>

namespace shiranui{
    template<typename T>
    struct VisitorForAST;
}

namespace shiranui{
    namespace syntax{
        namespace ast{
            namespace qi = boost::spirit::qi;
            namespace ph = boost::phoenix;
            // meta struct
//            struct LocationInfo;
//            struct Identifier; // should not use.
//            struct SourceCode;
//            struct Expression;
//            struct Statement;
//            // immediate value.
//
//            struct Number;
//            struct String;
//            struct Function;
//            struct Variable;
//
//            // expression
//            struct FunctionCall;
//            // struct IfElseExpression;
//
//            // statements.
//            struct Definement;
//            struct IfElseStatement;
//            struct ReturnStatement;
//            struct Block;
//
            struct LocationInfo{
                unsigned int line,column,length;
                virtual std::ostream& accept(VisitorForAST<std::ostream&>& visitor) = 0;
            };
            struct Expression : LocationInfo{
                virtual ~Expression() {};
                virtual std::ostream& accept(VisitorForAST<std::ostream&>& visitor) = 0;
            };
            struct Statement : LocationInfo{
                virtual ~Statement() {};
                virtual std::ostream& accept(VisitorForAST<std::ostream&>& visitor) = 0;
            };

            // metaelement.
            struct Identifier : LocationInfo{
                std::string name;
                // whats this?
                Identifier() : name("") {}
                explicit Identifier(std::string n);
                explicit Identifier(std::vector<char> n);
                bool operator<(const Identifier& id) const;
                std::ostream& accept(VisitorForAST<std::ostream&>& visitor);
                // Why can't use template...
            };

            // immediate values.
            struct Variable : Expression{
                Identifier value;
                explicit Variable(Identifier v);
                std::ostream& accept(VisitorForAST<std::ostream&>& visitor);
            };
            struct Number : Expression{
                int value;
                explicit Number(int v);
                std::ostream& accept(VisitorForAST<std::ostream&>& visitor);
            };
            struct String : Expression{
                std::string value;
                explicit String(std::string v);
                explicit String(std::vector<char> v);
                std::ostream& accept(VisitorForAST<std::ostream&>& visitor);
            };
            struct Block : Statement{
                std::vector<sp<Statement>> statements;
                Block(std::vector<Statement*> ss);
                std::ostream& accept(VisitorForAST<std::ostream&>& visitor);
            };

            struct Function : Expression{
                std::vector<Identifier> parameters;
                sp<Block>               body;
                Function(std::vector<Identifier> params,Block* ss);
                std::ostream& accept(VisitorForAST<std::ostream&>& visitor);
            };

            // expression.
            struct FunctionCall : Expression{
                sp<Expression> function;
                std::vector<sp<Expression>> arguments;
                FunctionCall(Expression* i,std::vector<Expression*> as);
                std::ostream& accept(VisitorForAST<std::ostream&>& visitor);
            };

            struct BinaryOperator : Expression{
                std::string op;
                sp<Expression> left,right;
                BinaryOperator(std::string o,Expression* l,Expression* r);
                std::ostream& accept(VisitorForAST<std::ostream&>& visitor);
            };
            struct UnaryOperator : Expression{
                std::string op;
                sp<Expression> exp;
                UnaryOperator(std::string o,Expression* e);
                std::ostream& accept(VisitorForAST<std::ostream&>& visitor);
            };


            struct IfElseExpression : Expression{
                sp<Expression> pred;
                sp<Expression> ife;
                sp<Expression> elsee;
                IfElseExpression(Expression* p,Expression* ib,Expression* eb);
                std::ostream& accept(VisitorForAST<std::ostream&>& visitor);
            };

            // statement.
            struct Definement : Statement{
                Identifier id;
                sp<Expression> value;
                bool is_const;
                Definement(Identifier i,Expression *e,bool isc);
                std::ostream& accept(VisitorForAST<std::ostream&>& visitor);
            };
            struct IfElseStatement : Statement{
                sp<Expression> pred;
                sp<Block> ifblock;
                sp<Block> elseblock;
                IfElseStatement(Expression* e,Block* iblock);
                IfElseStatement(Expression* e,Block* iblock,Block* eblock);
                std::ostream& accept(VisitorForAST<std::ostream&>& visitor);
            };
            struct ReturnStatement : Statement{
                sp<Expression> val;
                ReturnStatement(Expression* e);
                std::ostream& accept(VisitorForAST<std::ostream&>& visitor);
            };
            struct SourceCode : LocationInfo{
                std::vector<sp<Statement>> statements;
                explicit SourceCode(std::vector<Statement*> ss);
                std::ostream& accept(VisitorForAST<std::ostream&>& visitor);
            };
        }
    }
}
namespace shiranui{
    template<typename T>
    struct VisitorForAST{
        virtual ~VisitorForAST(){};
        virtual T visit(syntax::ast::Identifier&)       = 0;
        virtual T visit(syntax::ast::Variable&)         = 0;
        virtual T visit(syntax::ast::Number&)           = 0;
        virtual T visit(syntax::ast::String&)           = 0;
        virtual T visit(syntax::ast::Block&)            = 0;
        virtual T visit(syntax::ast::Function&)         = 0;
        virtual T visit(syntax::ast::FunctionCall&)     = 0;
        virtual T visit(syntax::ast::BinaryOperator&)   = 0;
        virtual T visit(syntax::ast::UnaryOperator&)    = 0;
        virtual T visit(syntax::ast::IfElseExpression&) = 0;
        virtual T visit(syntax::ast::Definement&)       = 0;
        virtual T visit(syntax::ast::ReturnStatement&)  = 0;
        virtual T visit(syntax::ast::IfElseStatement&)  = 0;
        virtual T visit(syntax::ast::SourceCode&)       = 0;
    };
    struct PrettyPrinter : VisitorForAST<std::ostream&>{
        std::ostream& os;
        PrettyPrinter(std::ostream& o) : os(o) {};
        std::ostream& visit(syntax::ast::Identifier&);
        std::ostream& visit(syntax::ast::Variable&);
        std::ostream& visit(syntax::ast::Number&);
        std::ostream& visit(syntax::ast::String&);
        std::ostream& visit(syntax::ast::Block&);
        std::ostream& visit(syntax::ast::Function&);
        std::ostream& visit(syntax::ast::FunctionCall&);
        std::ostream& visit(syntax::ast::BinaryOperator&);
        std::ostream& visit(syntax::ast::UnaryOperator&);
        std::ostream& visit(syntax::ast::IfElseExpression&);
        std::ostream& visit(syntax::ast::Definement&);
        std::ostream& visit(syntax::ast::ReturnStatement&);
        std::ostream& visit(syntax::ast::IfElseStatement&);
        std::ostream& visit(syntax::ast::SourceCode&);
    };
}


namespace shiranui{
    namespace syntax{
        namespace ast{
            template<typename T>
            std::ostream& operator<<(std::ostream& os,T& s){
                PrettyPrinter p(os);
                return s.accept(p);
            }
        }
    }
}
#endif
