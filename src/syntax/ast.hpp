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
    namespace syntax{
        namespace ast{
            namespace qi = boost::spirit::qi;
            namespace ph = boost::phoenix;

            // meta struct
            struct LocationInfo;
            struct Identifier; // should not use.
            struct SourceCode;
            struct Expression;
            struct Statement;
            // immediate value.

            struct Number;
            struct String;
            struct Function;
            struct Variable;

            // expression
            struct FunctionCall;
            // struct IfElseExpression;

            // statements.
            struct Definement;
            struct IfElseStatement;

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
                explicit Identifier(std::string n) : name(n) {}
                explicit Identifier(std::vector<char> n) : name(n.begin(),n.end()) {}
                std::ostream& serialize(std::ostream &os) const{
                    return os << name;
                }
                bool operator<(const Identifier& id) const{
                    return name < id.name;
                }
            };

            // immediate values.
            struct Variable : Expression{
                Identifier value;
                explicit Variable(Identifier v) : value(v) {}
                std::ostream& serialize(std::ostream &os) const{
                    return os << value;
                }
            };
            struct Number : Expression{
                int value;
                explicit Number(int v):value(v) {}
                std::ostream& serialize(std::ostream &os) const{
                    return os << value;
                 }
            };
            struct String : Expression{
                std::string value;
                explicit String(std::string v):value(v) {}
                explicit String(std::vector<char> v):value(v.begin(),v.end()) {}
                std::ostream& serialize(std::ostream &os) const{
                    return os << value;
                }
            };
            struct Function : Expression{
                std::vector<Identifier> parameters;
                std::vector<sp<Statement>> body;
                Function(std::vector<Identifier> params,std::vector<Statement*> ss)
                    : parameters(params) {
                        for(Statement* s : ss){
                            body.push_back(sp<Statement>(s));
                        }
                    }

                std::ostream& serialize(std::ostream &os) const{
                    os << "\\(";
                    for(size_t i=0;i<parameters.size();i++){
                        os << parameters[i];
                        if(i != parameters.size()-1){
                            os << ",";
                        }
                    }
                    os << "){" << std::endl;
                    for(size_t i=0;i<body.size();i++){
                        // compileerror.due to cycle?
                        os << *(body[i]) << std::endl;
                    }
                    return os << "}";
                }
            };

            // expression.
            struct FunctionCall : Expression{
                sp<Expression> function;
                std::vector<sp<Expression>> arguments;
                FunctionCall(Expression* i,std::vector<Expression*> as){
                    function = sp<Expression>(i);
                    for(Expression* e : as){
                        arguments.push_back(sp<Expression>(e));
                    }
                }
                std::ostream& serialize(std::ostream &os) const{
                    os << *function << "(";
                    for(size_t i=0;i<arguments.size();i++){
                        os << *(arguments[i]);
                        if(i != arguments.size()-1){
                            os << ",";
                        }
                    }
                    return os << ")";
                }
            };
            struct BinaryOperator : Expression{
                std::string op;
                sp<Expression> left,right;
                BinaryOperator(std::string o,Expression* l,Expression* r){
                    left = sp<Expression>(l);
                    right = sp<Expression>(r);
                    op = o;
                }
                std::ostream& serialize(std::ostream &os) const{
                    os << "(" << *left << " " << op << " " << *right << ")";
                    return os;
                }
            };
            struct UnaryOperator : Expression{
                std::string op;
                sp<Expression> exp;
                UnaryOperator(std::string o,Expression* e){
                    op = o;
                    exp = sp<Expression>(e);
                }
                std::ostream& serialize(std::ostream &os) const{
                    os << "(" <<  op << "(" << *exp << ")" << ")";
                    return os;
                }
            };


            struct IfElseExpression : Expression{
                sp<Expression> pred;
                std::vector<sp<Statement>> ifblock;
                std::vector<sp<Statement>> elseblock;
                IfElseExpression(Expression* p,std::vector<Statement*> ib,
                        std::vector<Statement*> eb)
                    : pred(p){
                        for(Statement* i : ib){
                            ifblock.push_back(sp<Statement>(i));
                        }
                        for(Statement* e : eb){
                            elseblock.push_back(sp<Statement>(e));
                        }
                    }

                std::ostream& serialize(std::ostream &os) const{
                    os << "if " << *pred << " then" << std::endl;
                    for(const auto& s : ifblock){
                        os << *s << ";";
                    }
                    os << "else" << std::endl;
                    for(const auto& s : elseblock){
                        os << *s << ";";
                    }
                    return os;
                }
            };

            // statement.
            struct Definement : Statement{
                Identifier id;
                sp<Expression> value;
                bool is_const;
                Definement(Identifier i,Expression *e,bool isc)
                    : id(i),value(e),is_const(isc) {}
                std::ostream& serialize(std::ostream &os) const{
                    return os << "mut " << id << "-> " << *value;
                }
            };
            struct IfElseStatement : Statement{
                sp<Expression> pred;
                std::vector<sp<Statement>> ifblock;
                std::vector<sp<Statement>> elseblock;
                IfElseStatement(Expression* e,std::vector<Statement*> iblock)
                    : pred(e){
                        for(auto s : iblock){
                            ifblock.push_back(sp<Statement>(s));
                        }
                    }
                IfElseStatement(Expression* e,std::vector<Statement*> iblock,
                        std::vector<Statement*> eblock)
                    : pred(e){
                        for(auto s : iblock){
                            ifblock.push_back(sp<Statement>(s));
                        }
                        for(auto s : eblock){
                            elseblock.push_back(sp<Statement>(s));
                        }
                    }
                std::ostream& serialize(std::ostream &os) const{
                    os << "if " << *pred << " then" << std::endl;
                    for(const auto& s : ifblock){
                        os << *s << ";" << std::endl;
                    }
                    os << "else" << std::endl;
                    for(const auto& s : elseblock){
                        os << *s << ";" << std::endl;
                    }
                    return os;
                }
            };

            struct SourceCode : LocationInfo{
                std::vector<sp<Statement>> statements;
                explicit SourceCode(std::vector<Statement*> ss){
                    for(Statement* s : ss){
                        statements.push_back(sp<Statement>(s));
                    }
                }
                std::ostream& serialize(std::ostream &os) const{
                    for(const auto& s : statements){
                        os << *s << std::endl;
                    }
                    return os;
                }
            };
        }
    }
}

namespace shiranui{
    namespace syntax{
        namespace ast{
            std::ostream& operator<<(std::ostream& os,
                                     const shiranui::syntax::ast::LocationInfo& s){
                return s.serialize(os);
            }
        }
    }
}
#endif
