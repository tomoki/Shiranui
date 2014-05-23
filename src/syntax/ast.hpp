#ifndef AST_HPP_INCLUDED
#define AST_HPP_INCLUDED

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
            namespace manip{
                struct LocationInfoPrinter;
            }
            struct LocationInfo{
                unsigned int line,column,length;
                manip::LocationInfoPrinter printLoc() const;
            };
            struct Identifier : LocationInfo{
                std::string name;
            };
            struct VarDefinement : LocationInfo{
                Identifier id;
                int value;
            };
            struct ConstDefinement : LocationInfo{
                Identifier id;
                int value;
            };

            // spirit doesn't allow one member struct.
            // http://stackoverflow.com/questions/19823413/spirit-qi-attribute-propagation-issue-with-single-member-struct
            struct SourceCode : LocationInfo{
                // Identifier id;
                std::vector<ConstDefinement> assignments;
            };
            namespace manip{
                struct LocationInfoPrinter{
                    const LocationInfo& li;
                    LocationInfoPrinter(const LocationInfo& li_) : li(li_) {}
                    friend std::ostream& operator<<(std::ostream& os,
                                                    const LocationInfoPrinter& lip){
                        return os << lip.li.line << ":" << lip.li.column << ":" << lip.li.length;
                    }
                };
            }
            manip::LocationInfoPrinter LocationInfo::printLoc() const{
                return {*this};
            }
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
                          (std::vector<shiranui::syntax::ast::ConstDefinement>,assignments)
                          );

#endif
