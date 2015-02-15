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
            namespace DSL{
                struct VisitorForDSL;
            }
        }
    }
}
namespace shiranui{
    namespace syntax{
        namespace ast{
            struct LocationInfo{
                unsigned int point=0,length=0;
            };
            struct MainNode : LocationInfo{
                runtime::infomation::RuntimeInfomation runtime_info;
                virtual void accept(VisitorForAST&) = 0;
            };
            struct Expression : MainNode{
                virtual ~Expression() {};
            };
            struct Statement : MainNode{
                virtual ~Statement() {};
            };

            // metaelement.
            struct Identifier : MainNode{
                std::string name;
                // whats this?
                Identifier() : name("") {}
                explicit Identifier(std::string);
                explicit Identifier(std::vector<char>);
                bool operator<(const Identifier&) const;
                bool operator==(const Identifier&) const;
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
            struct Boolean: Expression{
                bool value;
                explicit Boolean(bool);
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

            struct FlyMark;
            // pre is unit -> unit
            // post is (type of return value) -> unit
            struct Block : Statement{
                std::vector<sp<Statement>> statements;
                std::vector<sp<Block>> pre;
                std::vector<sp<Block>> post;
                std::vector<sp<Block>> invariant;
                std::vector<sp<FlyMark>> flymarks;
                std::vector<Identifier> post_id;
                Block();
                void add_statement(sp<Statement>);
                void add_pre(sp<Block>);
                void add_post(Identifier i,sp<Block>);
                void add_invariant(sp<Block>);
                void add_flymark(sp<FlyMark>);
                void accept(VisitorForAST&);
            };


            struct Function : Expression{
                Identifier lambda_id;
                std::vector<Identifier> parameters;
                sp<Block>               body;
                Function(Identifier,std::vector<Identifier>,sp<Block>);
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
            struct ExpressionStatement : Statement{
                sp<Expression> exp;
                ExpressionStatement(sp<Expression>);
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
            struct ProbeStatement : Statement{
                sp<Expression> value;
                ProbeStatement(sp<Expression>);
                void accept(VisitorForAST&);
            };
            struct AssertStatement : Statement{
                sp<Expression> value;
                AssertStatement(sp<Expression>);
                void accept(VisitorForAST&);
            };
            struct FlyLine : MainNode{
            };
            struct TestFlyLine : FlyLine{
                sp<Expression> left,right,error;
                explicit TestFlyLine(sp<Expression>,sp<Expression>,sp<Expression>);
                void accept(VisitorForAST&);
            };
            struct IdleFlyLine : FlyLine{
                sp<Expression> left,right;
                explicit IdleFlyLine(sp<Expression>,sp<Expression>);
                void accept(VisitorForAST&);
            };
            struct FlyMark : MainNode{
                sp<Expression> left;
                std::vector<sp<Expression>> right;
                explicit FlyMark(sp<Expression>);
                FlyMark(sp<Expression>,std::vector<sp<Expression>>);
                void accept(VisitorForAST&);
            };
            struct SourceCode : MainNode{
                std::vector<sp<Statement>> statements;
                std::vector<sp<FlyLine>> flylines;
                std::map<sp<Block>,sp<Function> > where_is_function_from;
                // FIXME: sp<fucntion> is NOT exact.It's copy.
                std::map<Identifier,sp<Function> > marker_to_lambda;
                explicit SourceCode(std::vector<sp<Statement>>);
                SourceCode();
                ~SourceCode();
                void add_statement(sp<Statement>);
                void add_flyline(sp<FlyLine>);
                void accept(VisitorForAST&);
            };

            namespace DSL{
                struct DSLInner : LocationInfo{
                    virtual void accept(VisitorForDSL&) = 0;
                };
                struct DSLVariable : DSLInner{
                    std::string name;
                    DSLVariable();
                    explicit DSLVariable(std::string);
                    explicit DSLVariable(std::vector<char>);
                    bool operator<(const DSLVariable&) const;
                    void accept(VisitorForDSL&);
                };
                struct DSLDefine : DSLInner{
                    sp<DSLVariable> var;
                    sp<DSLInner> value;
                    DSLDefine(sp<DSLVariable>,sp<DSLInner>);
                    void accept(VisitorForDSL&);
                };
                struct DSLImmediate : DSLInner{
                };
                struct DSLInteger : DSLImmediate{
                    int value;
                    explicit DSLInteger(int);
                    void accept(VisitorForDSL&);
                };
                struct DSLBoolean : DSLImmediate{
                    bool value;
                    explicit DSLBoolean(bool);
                    void accept(VisitorForDSL&);
                };
                struct DSLString : DSLImmediate{
                    std::string value;
                    explicit DSLString(std::string);
                    void accept(VisitorForDSL&);
                };
                struct DSLArray : DSLImmediate{
                    std::vector<sp<DSLInner> > value;
                    explicit DSLArray(std::vector<sp<DSLInner>>);
                    explicit DSLArray();
                    void accept(VisitorForDSL&);
                };
                struct DSLRef : DSLImmediate{
                    sp<DSLInner> to;
                    explicit DSLRef(sp<DSLInner>);
                    void accept(VisitorForDSL&);
                };
                struct DSLFunction : DSLImmediate{
                    typedef std::vector<std::pair<sp<ast::DSL::DSLVariable>,
                                                  sp<ast::DSL::DSLInner> > > myenv;
                    myenv environment;
                    Identifier lambda_id;
                    DSLFunction(myenv,sp<DSLVariable>);
                    void accept(VisitorForDSL&);
                };
                struct DataDSL : Expression{
                    sp<DSLInner> inner;
                    explicit DataDSL(sp<DSLInner>);
                    void accept(VisitorForAST&);
                };
            }
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
                virtual void visit(Boolean&)          = 0;
                virtual void visit(Enum&)             = 0;
                virtual void visit(Interval&)         = 0;
                virtual void visit(Block&)            = 0;
                virtual void visit(Function&)         = 0;
                virtual void visit(FunctionCall&)     = 0;
                virtual void visit(BinaryOperator&)   = 0;
                virtual void visit(UnaryOperator&)    = 0;
                virtual void visit(IfElseExpression&) = 0;
                virtual void visit(Definement&)       = 0;
                virtual void visit(ExpressionStatement&) = 0;
                virtual void visit(ReturnStatement&)  = 0;
                virtual void visit(ProbeStatement&)   = 0;
                virtual void visit(AssertStatement&)  = 0;
                virtual void visit(IfElseStatement&)  = 0;
                virtual void visit(ForStatement&)     = 0;
                virtual void visit(Assignment&)       = 0;
                virtual void visit(TestFlyLine&)      = 0;
                virtual void visit(IdleFlyLine&)      = 0;
                virtual void visit(FlyMark&)          = 0;
                virtual void visit(SourceCode&)       = 0;
                virtual void visit(DSL::DataDSL&)     = 0;
            };
            struct PrettyPrinterForAST : VisitorForAST{
                std::ostream& os;
                int indent;
                PrettyPrinterForAST(std::ostream& o) : os(o),indent(0) {};
                void visit(Identifier&);
                void visit(Variable&);
                void visit(Number&);
                void visit(String&);
                void visit(Boolean&);
                void visit(Enum&);
                void visit(Interval&);
                void visit(Block&);
                void visit(Function&);
                void visit(FunctionCall&);
                void visit(BinaryOperator&);
                void visit(UnaryOperator&);
                void visit(IfElseExpression&);
                void visit(Definement&);
                void visit(ExpressionStatement&);
                void visit(ReturnStatement&);
                void visit(ProbeStatement&);
                void visit(AssertStatement&);
                void visit(IfElseStatement&);
                void visit(ForStatement&);
                void visit(Assignment&);
                void visit(TestFlyLine&);
                void visit(IdleFlyLine&);
                void visit(FlyMark&);
                void visit(SourceCode&);
                void visit(DSL::DataDSL&);
                std::string ind();
            };
        }
    }
}
namespace shiranui{
    namespace syntax{
        namespace ast{
            namespace DSL{
                struct VisitorForDSL{
                    virtual void operator()(DSLVariable&)   = 0;
                    virtual void operator()(DSLDefine&)     = 0;
                    virtual void operator()(DSLInteger&)    = 0;
                    virtual void operator()(DSLBoolean&)    = 0;
                    virtual void operator()(DSLString&)     = 0;
                    virtual void operator()(DSLArray&)      = 0;
                    virtual void operator()(DSLRef&)        = 0;
                    virtual void operator()(DSLFunction&)   = 0;
                };
            }
        }
    }
}
#endif
