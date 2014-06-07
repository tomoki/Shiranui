#ifndef AST_HPP_INCLUDED
#define AST_HPP_INCLUDED

#include "../misc.hpp"
#include <iostream>
#include <memory>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_fusion.hpp>
#include <boost/fusion/include/adapt_struct.hpp>

#include <string>
#include <vector>

namespace shiranui{
    struct VisitorForAST;
}

namespace shiranui{
    namespace syntax{
        namespace ast{
            namespace qi = boost::spirit::qi;
            namespace ph = boost::phoenix;
            struct LocationInfo{
                unsigned int line,column,length;
                virtual void accept(VisitorForAST&) = 0;
            };
            struct Expression : LocationInfo{
                virtual ~Expression() {};
            };
            struct Statement : LocationInfo{
                virtual ~Statement() {};
            };

            // metaelement.
            struct Identifier : LocationInfo{
                std::string name;
                // whats this?
                Identifier() : name("") {}
                explicit Identifier(std::string n);
                explicit Identifier(std::vector<char> n);
                bool operator<(const Identifier& id) const;
                void accept(VisitorForAST&);
            };

            // immediate values.
            struct Variable : Expression{
                Identifier value;
                explicit Variable(Identifier v);
                void accept(VisitorForAST&);
            };
            struct Number : Expression{
                int value;
                explicit Number(int v);
                void accept(VisitorForAST&);
            };
            struct String : Expression{
                std::string value;
                explicit String(std::string v);
                explicit String(std::vector<char> v);
                void accept(VisitorForAST&);
            };
            struct Block : Statement{
                std::vector<sp<Statement>> statements;
                Block(std::vector<Statement*> ss);
                void accept(VisitorForAST&);
            };

            struct Function : Expression{
                std::vector<Identifier> parameters;
                sp<Block>               body;
                Function(std::vector<Identifier> params,Block* ss);
                void accept(VisitorForAST&);
            };

            // expression.
            struct FunctionCall : Expression{
                sp<Expression> function;
                std::vector<sp<Expression>> arguments;
                FunctionCall(Expression* i,std::vector<Expression*> as);
                void accept(VisitorForAST&);
            };

            struct BinaryOperator : Expression{
                std::string op;
                sp<Expression> left,right;
                BinaryOperator(std::string o,Expression* l,Expression* r);
                void accept(VisitorForAST&);
            };
            struct UnaryOperator : Expression{
                std::string op;
                sp<Expression> exp;
                UnaryOperator(std::string o,Expression* e);
                void accept(VisitorForAST&);
            };


            struct IfElseExpression : Expression{
                sp<Expression> pred;
                sp<Expression> ife;
                sp<Expression> elsee;
                IfElseExpression(Expression* p,Expression* ib,Expression* eb);
                void accept(VisitorForAST&);
            };

            // statement.
            struct Definement : Statement{
                Identifier id;
                sp<Expression> value;
                bool is_const;
                Definement(Identifier i,Expression *e,bool isc);
                void accept(VisitorForAST&);
            };
            struct IfElseStatement : Statement{
                sp<Expression> pred;
                sp<Block> ifblock;
                sp<Block> elseblock;
                IfElseStatement(Expression* e,Block* iblock);
                IfElseStatement(Expression* e,Block* iblock,Block* eblock);
                void accept(VisitorForAST&);
            };
            struct ReturnStatement : Statement{
                sp<Expression> value;
                ReturnStatement(Expression* e);
                void accept(VisitorForAST&);
            };
            struct SourceCode : LocationInfo{
                std::vector<sp<Statement>> statements;
                explicit SourceCode(std::vector<Statement*> ss);
                void accept(VisitorForAST&);
            };
        }
    }
}
namespace shiranui{
    struct VisitorForAST{
        virtual ~VisitorForAST(){};
        virtual void visit(syntax::ast::Identifier&)       = 0;
        virtual void visit(syntax::ast::Variable&)         = 0;
        virtual void visit(syntax::ast::Number&)           = 0;
        virtual void visit(syntax::ast::String&)           = 0;
        virtual void visit(syntax::ast::Block&)            = 0;
        virtual void visit(syntax::ast::Function&)         = 0;
        virtual void visit(syntax::ast::FunctionCall&)     = 0;
        virtual void visit(syntax::ast::BinaryOperator&)   = 0;
        virtual void visit(syntax::ast::UnaryOperator&)    = 0;
        virtual void visit(syntax::ast::IfElseExpression&) = 0;
        virtual void visit(syntax::ast::Definement&)       = 0;
        virtual void visit(syntax::ast::ReturnStatement&)  = 0;
        virtual void visit(syntax::ast::IfElseStatement&)  = 0;
        virtual void visit(syntax::ast::SourceCode&)       = 0;
    };
    struct PrettyPrinter : VisitorForAST{
        std::ostream& os;
        PrettyPrinter(std::ostream& o) : os(o) {};
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


namespace shiranui{
    namespace syntax{
        namespace ast{
            template<typename T>
            std::ostream& operator<<(std::ostream& os,T& s){
                PrettyPrinter p(os);
                s.accept(p);
                return os;
            }
        }
    }
}
#endif
