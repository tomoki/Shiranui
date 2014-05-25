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
            struct VarDefinement;
            struct ConstDefinement;
            struct IfStatement;

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
                Identifier(std::string n) : name(n) {}
                Identifier(std::vector<char> n) : name(n.begin(),n.end()) {}
                std::ostream& serialize(std::ostream &os) const{
                    return os << name;
                }
            };

            // immediate values.
            struct Variable : Expression{
                Identifier value;
                Variable(Identifier v) : value(v) {}
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
                     os << "){";
                     for(size_t i=0;i<body.size();i++){
                         // compileerror.due to cycle?
                         os << *(body[i]) << std::endl;
                     }
                     return os << "}";
                 }
             };
 
             // expression.
             struct FunctionCall : Expression{
                 Identifier function_name;
                 std::vector<sp<Expression>> arguments;
                 FunctionCall(Identifier i,std::vector<Expression*> as)
                     : function_name(i){
                         for(Expression* e : as){
                             arguments.push_back(sp<Expression>(e));
                         }
                 }
                 std::ostream& serialize(std::ostream &os) const{
                     os << function_name << "(";
                     for(size_t i=0;i<arguments.size();i++){
                         os << *(arguments[i]);
                         if(i != arguments.size()-1){
                             os << ",";
                         }
                     }
                     return os << ")";
                 }
             };

//             struct IfElseExpression : Expression{
//                 sp<Expression> pred;
//                 std::vector<sp<Statement>> ifblock;
//                 std::vector<sp<Statement>> elseblock;
// 
//                 std::ostream& serialize(std::ostream &os) const{
//                     os << "if " << pred << " then" << std::endl;
//                     for(const Statement& s : ifblock){
//                         os << s << ";";
//                     }
//                     os << "else" << std::endl;
//                     for(const Statement& s : elseblock){
//                         os << s << ";";
//                     }
//                     return os;
//                 }
//             };
// 

            // statement.
            struct VarDefinement : Statement{
                Identifier id;
                sp<Expression> value;
                VarDefinement(Identifier i,Expression *e)
                     : id(i),value(e) {}
                std::ostream& serialize(std::ostream &os) const{
                    return os << "mut " << id << "-> " << *value;
                }
            };
            struct ConstDefinement : Statement{
                Identifier id;
                sp<Expression> value;
                ConstDefinement(Identifier i,Expression *e) 
                     : id(i),value(e) {}
                std::ostream& serialize(std::ostream &os) const{
                    return os << "let " << id << "-> " << *value;
                }
            };
            struct IfStatement : Statement{
                sp<Expression> pred;
                std::vector<sp<Statement>> ifblock;
                IfStatement(Expression* e,std::vector<Statement*> iblock)
                    : pred(e){
                        for(auto s : iblock){
                            ifblock.push_back(sp<Statement>(s));
                        }
                }
                std::ostream& serialize(std::ostream &os) const{
                    os << "if " << *pred << " then" << std::endl;
                    for(const auto& s : ifblock){
                        os << *s << ";";
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
            std::ostream& operator<<(std::ostream& os,
                                     const sp<shiranui::syntax::ast::LocationInfo>& s){
                return s->serialize(os);
            }
        }
    }
}
#endif
