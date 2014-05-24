#ifndef AST_HPP_INCLUDED
#define AST_HPP_INCLUDED

#include "../misc.hpp"
#include <iostream>
#include <boost/variant.hpp>
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

            // immediate value.
            struct Number;
            struct String;
            struct Function;
            struct Variable;

            // expression
            struct FunctionCall;

            // statements.
            struct VarDefinement;
            struct ConstDefinement;



            typedef boost::variant<Number,
                                   String,
                                   Function,
                                   Variable,
                                   FunctionCall> Expression;

            typedef boost::variant<VarDefinement,
                                   ConstDefinement> Statement;

            struct LocationInfo{
                unsigned int line,column,length;
                virtual std::ostream& serialize(std::ostream&) const = 0;
                friend std::ostream& operator<<(std::ostream&,const LocationInfo&);
            };

            // metaelement.
            struct Identifier : LocationInfo{
                std::string name;
                std::ostream& serialize(std::ostream &os) const{
                    return os << name;
                }
            };

            // immediate values.
            struct Variable : LocationInfo{
                Identifier value;
                std::ostream& serialize(std::ostream &os) const{
                    return os << value;
                }
            };
            struct Number : LocationInfo{
                int value;
                std::ostream& serialize(std::ostream &os) const{
                    return os << value;
                }
            };
            struct String : LocationInfo{
                std::string value;
                std::ostream& serialize(std::ostream &os) const{
                    return os << value;
                }
            };
            struct Function : LocationInfo{
                std::vector<Identifier> parameters;
                std::vector<Statement> body;
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
                        // os << body[i] << std::endl;
                        os << "hoge" << std::endl;
                    }
                    return os << "}";
                }
            };

            // expression.
            struct FunctionCall : LocationInfo{
                Identifier function_name;
                std::vector<Expression> arguments;
                std::ostream& serialize(std::ostream &os) const{
                    os << function_name << "(";
                    for(size_t i=0;i<arguments.size();i++){
                        os << arguments[i];
                        if(i != arguments.size()-1){
                            os << ",";
                        }
                    }
                    return os << ")";
                }
            };
            // statement.
            struct VarDefinement : LocationInfo{
                Identifier id;
                Expression value;
                std::ostream& serialize(std::ostream &os) const{
                    return os << "mut " << id << "-> " << value;
                }
            };
            struct ConstDefinement : LocationInfo{
                Identifier id;
                Expression value;
                std::ostream& serialize(std::ostream &os) const{
                    return os << "let " << id << "-> " << value;
                }
            };

            struct SourceCode : LocationInfo{
                std::vector<Statement> statements;
                std::ostream& serialize(std::ostream &os) const{
                    return os << statements.size();
                }
            };
        }
     }
}

// meta structs
BOOST_FUSION_ADAPT_STRUCT(shiranui::syntax::ast::Identifier,
                          (std::string,name));
// immeidate value (also,it is expression).
BOOST_FUSION_ADAPT_STRUCT(shiranui::syntax::ast::Number,
                          (int,value));
BOOST_FUSION_ADAPT_STRUCT(shiranui::syntax::ast::String,
                          (std::string,value));
BOOST_FUSION_ADAPT_STRUCT(shiranui::syntax::ast::Variable,
                          (shiranui::syntax::ast::Identifier,value));
// BOOST_FUSION_ADAPT_STRUCT(shiranui::syntax::ast::Function,
//                           (std::vector<shiranui::syntax::ast::Identifier>,parameters)
//                           (std::vector<shiranui::syntax::ast::Statement>,body));
BOOST_FUSION_ADAPT_STRUCT(shiranui::syntax::ast::Function,
                          (std::vector<shiranui::syntax::ast::Identifier>,parameters)
                          (std::vector<shiranui::syntax::ast::Statement>,body));
                          // );

// expression
BOOST_FUSION_ADAPT_STRUCT(shiranui::syntax::ast::FunctionCall,
                          (shiranui::syntax::ast::Identifier,function_name)
                          (std::vector<shiranui::syntax::ast::Expression>,arguments));
// statements.
BOOST_FUSION_ADAPT_STRUCT(shiranui::syntax::ast::VarDefinement,
                          (shiranui::syntax::ast::Identifier,id)
                          (shiranui::syntax::ast::Expression,value));
BOOST_FUSION_ADAPT_STRUCT(shiranui::syntax::ast::ConstDefinement,
                          (shiranui::syntax::ast::Identifier,id)
                          (shiranui::syntax::ast::Expression,value));

BOOST_FUSION_ADAPT_STRUCT(shiranui::syntax::ast::SourceCode,
                          (std::vector<shiranui::syntax::ast::Statement>,statements)
                          );


namespace shiranui{
    namespace syntax{
        namespace ast{
            // struct expression_serializer : boost::static_visitor<std::ostream&>{
            //     std::ostream& operator()(const LocationInfo& s,std::ostream& os) const{
            //         return s.serialize(os);
            //     }
            // };
            std::ostream& operator<<(std::ostream& os,
                                     const shiranui::syntax::ast::LocationInfo& s){
                return s.serialize(os);
            }
            // std::ostream& operator<<(std::ostream& os,
            //                          const shiranui::syntax::ast::Expression& e){
            //     // return e.serialize(os);
            //     return boost::apply_visitor(expression_serializer(),e,os);
            // }
        }
    }
}
#endif
