#ifndef AST_HPP_INCLUDED
#define AST_HPP_INCLUDED

#include "../misc.hpp"
#include <iostream>
#include <memory>

#include <string>
#include <vector>

namespace shiranui{
    namespace syntax{
        namespace ast{
            struct VisitorForAST;
        }
    }
}

namespace shiranui{
    namespace syntax{
        namespace ast{
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

            struct Array : Expression{
                virtual ~Array() {};
            };
//            struct Interval : Array{
//                sp<Expression> start,end,interval;
//                explicit Interval(sp<Expression>,sp<Expression>);
//                // Interval(sp<Expression>,sp<Expression>,sp<Expression>);
//            };
            struct Enum : Array{
                std::vector<sp<Expression>> expressions;
                explicit Enum(std::vector<sp<Expression>>);
                explicit Enum();
                void accept(VisitorForAST&);
            };

            struct Block : Statement{
                std::vector<sp<Statement>> statements;
                Block(std::vector<sp<Statement>> ss);
                void accept(VisitorForAST&);
            };

            struct Function : Expression{
                std::vector<Identifier> parameters;
                sp<Block>               body;
                Function(std::vector<Identifier> params,sp<Block> ss);
                void accept(VisitorForAST&);
            };

            // expression.
            struct FunctionCall : Expression{
                sp<Expression> function;
                std::vector<sp<Expression>> arguments;
                FunctionCall(sp<Expression> i,std::vector<sp<Expression>> as);
                void accept(VisitorForAST&);
            };

            struct BinaryOperator : Expression{
                std::string op;
                sp<Expression> left,right;
                BinaryOperator(std::string o,sp<Expression> l,sp<Expression> r);
                void accept(VisitorForAST&);
            };
            struct UnaryOperator : Expression{
                std::string op;
                sp<Expression> exp;
                UnaryOperator(std::string o,sp<Expression> e);
                void accept(VisitorForAST&);
            };

            struct IfElseExpression : Expression{
                sp<Expression> pred;
                sp<Expression> ife;
                sp<Expression> elsee;
                IfElseExpression(sp<Expression> p,sp<Expression> ib,sp<Expression> eb);
                void accept(VisitorForAST&);
            };

            // statement.
            struct Definement : Statement{
                Identifier id;
                sp<Expression> value;
                bool is_const;
                Definement(Identifier i,sp<Expression> e,bool isc);
                void accept(VisitorForAST&);
            };
            struct IfElseStatement : Statement{
                sp<Expression> pred;
                sp<Block> ifblock;
                sp<Block> elseblock;
                IfElseStatement(sp<Expression> e,sp<Block> iblock);
                IfElseStatement(sp<Expression> e,sp<Block> iblock,sp<Block> eblock);
                void accept(VisitorForAST&);
            };
            struct Assignment : Statement{
                Identifier id;
                sp<Expression> value;
                Assignment(Identifier,sp<Expression>);
                void accept(VisitorForAST&);
            };
            struct ReturnStatement : Statement{
                sp<Expression> value;
                ReturnStatement(sp<Expression> e);
                void accept(VisitorForAST&);
            };
            struct FlyLine : LocationInfo{
                sp<Expression> left,right;
                explicit FlyLine(sp<Expression>);
                explicit FlyLine(sp<Expression>,sp<Expression>);
                void accept(VisitorForAST&);
            };
            struct SourceCode : LocationInfo{
                std::vector<sp<Statement>> statements;
                std::vector<sp<FlyLine>> flylines;
                explicit SourceCode(std::vector<sp<Statement>> ss);
                SourceCode();
                void add_statement(sp<Statement> s);
                void add_flyline(sp<FlyLine> s);
                void accept(VisitorForAST&);
            };
        }
    }
}
namespace shiranui{
    namespace syntax{
        namespace ast{
            struct VisitorForAST{
                virtual ~VisitorForAST() {};
                virtual void visit(Identifier&)       = 0;
                virtual void visit(Variable&)         = 0;
                virtual void visit(Number&)           = 0;
                virtual void visit(String&)           = 0;
                virtual void visit(Enum&)             = 0;
                virtual void visit(Block&)            = 0;
                virtual void visit(Function&)         = 0;
                virtual void visit(FunctionCall&)     = 0;
                virtual void visit(BinaryOperator&)   = 0;
                virtual void visit(UnaryOperator&)    = 0;
                virtual void visit(IfElseExpression&) = 0;
                virtual void visit(Definement&)       = 0;
                virtual void visit(ReturnStatement&)  = 0;
                virtual void visit(IfElseStatement&)  = 0;
                virtual void visit(Assignment&)       = 0;
                virtual void visit(FlyLine&)          = 0;
                virtual void visit(SourceCode&)       = 0;
            };
            struct PrettyPrinterForAST : VisitorForAST{
                std::ostream& os;
                int indent;
                PrettyPrinterForAST(std::ostream& o) : os(o),indent(0) {};
                void visit(Identifier&);
                void visit(Variable&);
                void visit(Number&);
                void visit(String&);
                void visit(Enum&);
                void visit(Block&);
                void visit(Function&);
                void visit(FunctionCall&);
                void visit(BinaryOperator&);
                void visit(UnaryOperator&);
                void visit(IfElseExpression&);
                void visit(Definement&);
                void visit(ReturnStatement&);
                void visit(IfElseStatement&);
                void visit(Assignment&);
                void visit(FlyLine&);
                void visit(SourceCode&);
                std::string ind();
            };

        }
    }
}

#endif
