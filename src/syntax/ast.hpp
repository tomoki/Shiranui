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
                virtual std::ostream& serialize(std::ostream&) const = 0;
                friend std::ostream& operator<<(std::ostream&,const LocationInfo&);
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
                std::ostream& serialize(std::ostream &os) const;
                bool operator<(const Identifier& id) const;
            };

            // immediate values.
            struct Variable : Expression{
                Identifier value;
                explicit Variable(Identifier v);
                std::ostream& serialize(std::ostream &os) const;
            };
            struct Number : Expression{
                int value;
                explicit Number(int v);
                std::ostream& serialize(std::ostream &os) const;
            };
            struct String : Expression{
                std::string value;
                explicit String(std::string v);
                explicit String(std::vector<char> v);
                std::ostream& serialize(std::ostream &os) const;
            };
            struct Block : Statement{
                std::vector<sp<Statement>> statements;
                Block(std::vector<Statement*> ss);
                std::ostream& serialize(std::ostream &os) const;
            };

            struct Function : Expression{
                std::vector<Identifier> parameters;
                sp<Block>               body;
                Function(std::vector<Identifier> params,Block* ss);
                std::ostream& serialize(std::ostream &os) const;
            };

            // expression.
            struct FunctionCall : Expression{
                sp<Expression> function;
                std::vector<sp<Expression>> arguments;
                FunctionCall(Expression* i,std::vector<Expression*> as);
                std::ostream& serialize(std::ostream &os) const;
            };

            struct BinaryOperator : Expression{
                std::string op;
                sp<Expression> left,right;
                BinaryOperator(std::string o,Expression* l,Expression* r);
                std::ostream& serialize(std::ostream &os) const;
            };
            struct UnaryOperator : Expression{
                std::string op;
                sp<Expression> exp;
                UnaryOperator(std::string o,Expression* e);
                std::ostream& serialize(std::ostream &os) const;
            };


            struct IfElseExpression : Expression{
                sp<Expression> pred;
                sp<Expression> ife;
                sp<Expression> elsee;
                IfElseExpression(Expression* p,Expression* ib,Expression* eb);
                std::ostream& serialize(std::ostream &os) const;
            };

            // statement.
            struct Definement : Statement{
                Identifier id;
                sp<Expression> value;
                bool is_const;
                Definement(Identifier i,Expression *e,bool isc);
                std::ostream& serialize(std::ostream &os) const;
            };
            struct IfElseStatement : Statement{
                sp<Expression> pred;
                sp<Block> ifblock;
                sp<Block> elseblock;
                IfElseStatement(Expression* e,Block* iblock);
                IfElseStatement(Expression* e,Block* iblock,Block* eblock);
                std::ostream& serialize(std::ostream &os) const;
            };
            struct ReturnStatement : Statement{
                sp<Expression> val;
                ReturnStatement(Expression* e);
                std::ostream& serialize(std::ostream &os) const;
            };
            struct SourceCode : LocationInfo{
                std::vector<sp<Statement>> statements;
                explicit SourceCode(std::vector<Statement*> ss);
                std::ostream& serialize(std::ostream &os) const;
            };
        }
    }
}

namespace shiranui{
    namespace syntax{
        namespace ast{
            std::ostream& operator<<(std::ostream& os,
                                     const shiranui::syntax::ast::LocationInfo& s);
        }
    }
}
#endif
