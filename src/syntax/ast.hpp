#ifndef AST_HPP_INCLUDED
#define AST_HPP_INCLUDED

#include "../misc.hpp"
#include "../runtime/runtime_info.hpp"
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
                runtime::infomation::RuntimeInfomation runtime_info;
                unsigned int point,line,column,length;
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
            struct Interval : Array{
                sp<Expression> start,end,next;
                bool right_close;
                Interval(sp<Expression>,sp<Expression>,sp<Expression>,bool);
                void accept(VisitorForAST&);
            };
            struct Enum : Array{
                std::vector<sp<Expression>> expressions;
                explicit Enum(std::vector<sp<Expression>>);
                explicit Enum();
                void accept(VisitorForAST&);
            };

            struct Block : Statement{
                std::vector<sp<Statement>> statements;
                Block(std::vector<sp<Statement>>);
                void accept(VisitorForAST&);
            };

            struct Function : Expression{
                std::vector<Identifier> parameters;
                sp<Block>               body;
                Function(std::vector<Identifier>,sp<Block>);
                void accept(VisitorForAST&);
            };

            // expression.
            struct FunctionCall : Expression{
                sp<Expression> function;
                std::vector<sp<Expression>> arguments;
                FunctionCall(sp<Expression>,std::vector<sp<Expression>>);
                void accept(VisitorForAST&);
            };

            struct BinaryOperator : Expression{
                std::string op;
                sp<Expression> left,right;
                BinaryOperator(std::string,sp<Expression>,sp<Expression>);
                void accept(VisitorForAST&);
            };
            struct UnaryOperator : Expression{
                std::string op;
                sp<Expression> exp;
                UnaryOperator(std::string,sp<Expression>);
                void accept(VisitorForAST&);
            };

            struct IfElseExpression : Expression{
                sp<Expression> pred;
                sp<Expression> ife;
                sp<Expression> elsee;
                IfElseExpression(sp<Expression>,sp<Expression>,sp<Expression>);
                void accept(VisitorForAST&);
            };

            // statement.
            struct Definement : Statement{
                Identifier id;
                sp<Expression> value;
                bool is_const;
                Definement(Identifier,sp<Expression>,bool);
                void accept(VisitorForAST&);
            };
            struct IfElseStatement : Statement{
                sp<Expression> pred;
                sp<Block> ifblock;
                sp<Block> elseblock;
                IfElseStatement(sp<Expression>,sp<Block>);
                IfElseStatement(sp<Expression>,sp<Block>,sp<Block>);
                void accept(VisitorForAST&);
            };
            struct ForStatement : Statement{
                Identifier loop_var;
                sp<Expression> loop_exp;
                sp<Block> block;
                ForStatement(Identifier,sp<Expression>,sp<Block>);
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
                ReturnStatement(sp<Expression>);
                void accept(VisitorForAST&);
            };
            struct FlyLine : LocationInfo{
            };
            struct TestFlyLine : FlyLine{
                sp<Expression> left,right;
                explicit TestFlyLine(sp<Expression>,sp<Expression>);
                void accept(VisitorForAST&);
            };
            struct IdleFlyLine : FlyLine{
                sp<Expression> left,right;
                explicit IdleFlyLine(sp<Expression>,sp<Expression>);
                void accept(VisitorForAST&);
            };
            struct SourceCode : LocationInfo{
                std::vector<sp<Statement>> statements;
                std::vector<sp<FlyLine>> flylines;
                explicit SourceCode(std::vector<sp<Statement>>);
                SourceCode();
                void add_statement(sp<Statement>);
                void add_flyline(sp<FlyLine>);
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
                virtual void visit(Interval&)         = 0;
                virtual void visit(Block&)            = 0;
                virtual void visit(Function&)         = 0;
                virtual void visit(FunctionCall&)     = 0;
                virtual void visit(BinaryOperator&)   = 0;
                virtual void visit(UnaryOperator&)    = 0;
                virtual void visit(IfElseExpression&) = 0;
                virtual void visit(Definement&)       = 0;
                virtual void visit(ReturnStatement&)  = 0;
                virtual void visit(IfElseStatement&)  = 0;
                virtual void visit(ForStatement&)     = 0;
                virtual void visit(Assignment&)       = 0;
                virtual void visit(TestFlyLine&)      = 0;
                virtual void visit(IdleFlyLine&)      = 0;
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
                void visit(Interval&);
                void visit(Block&);
                void visit(Function&);
                void visit(FunctionCall&);
                void visit(BinaryOperator&);
                void visit(UnaryOperator&);
                void visit(IfElseExpression&);
                void visit(Definement&);
                void visit(ReturnStatement&);
                void visit(IfElseStatement&);
                void visit(ForStatement&);
                void visit(Assignment&);
                void visit(TestFlyLine&);
                void visit(IdleFlyLine&);
                void visit(SourceCode&);
                std::string ind();
            };

        }
    }
}

#endif
