#ifndef AST_HPP_INCLUDED
#define AST_HPP_INCLUDED

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

            struct LocationInfo;
            struct Identifier;
            struct VarDefinement;
            struct ConstDefinement;
            struct SourceCode;

            typedef boost::variant<VarDefinement,
                                   ConstDefinement> Statement;
            struct LocationInfo{
                unsigned int line,column,length;
                virtual std::ostream& serialize(std::ostream&) const = 0;
            };
            struct Identifier : LocationInfo{
                std::string name;
                std::ostream& serialize(std::ostream &os) const{
                    return os << name;
                }
            };
            struct VarDefinement : LocationInfo{
                Identifier id;
                int value;
                std::ostream& serialize(std::ostream &os) const{
                    return os << "mut " << id.name << "-> " << value;
                }
            };
            struct ConstDefinement : LocationInfo{
                Identifier id;
                int value;
                std::ostream& serialize(std::ostream &os) const{
                    return os << "let " << id.name << "-> " << value;
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
BOOST_FUSION_ADAPT_STRUCT(shiranui::syntax::ast::Identifier,
                          (std::string,name));
BOOST_FUSION_ADAPT_STRUCT(shiranui::syntax::ast::VarDefinement,
                          (shiranui::syntax::ast::Identifier,id)
                          (int,value));
BOOST_FUSION_ADAPT_STRUCT(shiranui::syntax::ast::ConstDefinement,
                          (shiranui::syntax::ast::Identifier,id)
                          (int,value));
BOOST_FUSION_ADAPT_STRUCT(shiranui::syntax::ast::SourceCode,
                          (std::vector<shiranui::syntax::ast::Statement>,statements)
                          );


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
