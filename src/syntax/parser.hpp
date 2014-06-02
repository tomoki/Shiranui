#ifndef PARSER_HPP_INCLUDED
#define PARSER_HPP_INCLUDED

// #define BOOST_SPIRIT_DEBUG
#define BOOST_SPIRIT_USE_PHOENIX_V3
#include <boost/spirit/include/qi.hpp>

#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_stl.hpp>
#include <boost/spirit/include/support_line_pos_iterator.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_fusion.hpp>

#include <boost/fusion/include/io.hpp>

#include <iostream>
#include <iomanip>
#include <string>

#include "ast.hpp"
#include "../misc.hpp"

// to put line,column info to AST,see following url.
// http://stackoverflow.com/questions/19612657/boostspirit-access-position-iterator-from-semantic-actions
namespace shiranui{
    namespace syntax{
        namespace qi = boost::spirit::qi;
        namespace ph = boost::phoenix;
        namespace ascii = boost::spirit::ascii;
        typedef boost::spirit::line_pos_iterator<std::string::const_iterator>
                                                               pos_iterator_t;


        struct error_handler_f{
            typedef qi::error_handler_result result_type;
            // does'nt work currently.(may need space skipping?)
            // get_line,get_column is defined in
            //   http://www.boost.org/doc/libs/1_47_0/boost/spirit/home/support/iterators/line_pos_iterator.hpp

            template<typename T1,typename T2,typename T3,typename T4>
            result_type operator()(T1 b,T2 e,T3 where,const T4& what) const{
                std::cerr << "Error expecting " << what << " in line " << get_line(where) << ":" << std::endl
                          << std::string(b,e) << std::endl
                          << std::setw(std::distance(b,where)) << "^---- here" << std::endl;
                return qi::fail;
            }
        };

        template<typename Iterator>
        struct annotation_f{
            typedef void result_type;
            const Iterator first;
            annotation_f(Iterator first_) : first(first_){}
            template<typename Val,typename First,typename Last>
            void operator()(Val& v,First f,Last l) const{
                do_annotate(v,f,l,first);
            }
        private:
            void static do_annotate(ast::LocationInfo& li,Iterator f,Iterator l,Iterator first){
                using std::distance;
                li.line = get_line(f);
                li.column = get_column(first,f);
                li.length = distance(f,l);
            }
            void static do_annotate(ast::LocationInfo* li,Iterator f,Iterator l,Iterator first){
                using std::distance;
                li->line = get_line(f);
                li->column = get_column(first,f);
                li->length = distance(f,l);
            }

            static void do_annotate(...){
                std::cerr << "(not having LocationInfo)" << std::endl;
            }
        };

        template<typename Iterator=pos_iterator_t,typename Skipper=qi::space_type>
        struct Parser : public qi::grammar<Iterator,ast::SourceCode*(),Skipper>{
            // change const_definement to sourcecode.
            Parser(Iterator first) : Parser::base_type(source),
                                     annotate(first){
                using namespace qi;
                identifier = as_string[(alpha >> *(alnum | char_('_')))];
                integer    = int_ [qi::_val = ph::new_<ast::Number>(qi::_1)];
                variable   = identifier [qi::_val = ph::new_<ast::Variable>(qi::_1)];
                function   = (lit("\\") >> "(" >> (identifier % ",") >> ")"
                                  >> "{" >> *statement >> "}")
                              [qi::_val = ph::new_<ast::Function>(qi::_1,qi::_2)];

                definement = ("let" >> identifier >> "=" >> expression)
                              [qi::_val = ph::new_<ast::Definement>(qi::_1,qi::_2,true)]
                           | ("mut" >> identifier >> "=" >> expression)
                              [qi::_val = ph::new_<ast::Definement>(qi::_1,qi::_2,false)]
                           ;
                ifelse_stmt= ("if" >> expression 
                                   >> "{" >> *statement >> "}"
                                   >> "else" >> "{" >> *statement >> "}")
                              [qi::_val = ph::new_<ast::IfElseStatement>(qi::_1,qi::_2,qi::_3)]
                           | ("if" >> expression
                                   >> "{" >> *statement >> "}")
                              [qi::_val = ph::new_<ast::IfElseStatement>(qi::_1,qi::_2)]
                           ;
                statement  = (definement >> ";")
                           | ifelse_stmt
                           ;
                expression = test [qi::_val = qi::_1];

                test       = or_test [qi::_val = qi::_1];
                or_test    = and_test [qi::_val = qi::_1] >>
                              *(("or" >> and_test)
                               [qi::_val = ph::new_<ast::BinaryOperator>("or",qi::_val,qi::_1)])
                           ;
                and_test   = not_test [qi::_val = qi::_1] >> 
                             *(("and" >> not_test)
                               [qi::_val = ph::new_<ast::BinaryOperator>("and",qi::_val,qi::_1)])
                           ;
                not_test   = ("not" >> not_test)
                              [qi::_val = ph::new_<ast::UnaryOperator>("not",qi::_1)]
                           | comparison [qi::_val = qi::_1]
                           ;

                // TODO add >,<,<=,>=,...
                comparison = addi [qi::_val = qi::_1] >> 
                             *(("=" >> addi)
                               [qi::_val = ph::new_<ast::BinaryOperator>("=",qi::_val,qi::_1)]
                             | ("/=" >> multi)
                               [qi::_val = ph::new_<ast::BinaryOperator>("/=",qi::_val,qi::_1)]
                              )
                           ;

                addi       = multi [qi::_val = qi::_1] >> 
                             *(('+' >> addi)
                               [qi::_val = ph::new_<ast::BinaryOperator>("+",qi::_val,qi::_1)]
                             | ('-' >> multi)
                               [qi::_val = ph::new_<ast::BinaryOperator>("-",qi::_val,qi::_1)]
                              )
                           ;

                multi      = unary [qi::_val = qi::_1] >>
                             *(('*' >> unary)
                               [qi::_val = ph::new_<ast::BinaryOperator>("*",qi::_val,qi::_1)]
                             | ('/' >> unary)
                               [qi::_val = ph::new_<ast::BinaryOperator>("/",qi::_val,qi::_1)]
                              )
                           ;
                // .alias() not work.
                unary      = power [qi::_val = qi::_1]
                           | ("+" >> unary)
                              [qi::_val = ph::new_<ast::UnaryOperator>("+",qi::_1)]
                           | ("-" >> unary)
                              [qi::_val = ph::new_<ast::UnaryOperator>("-",qi::_1)]
                           ;
                power      = atom [qi::_val = qi::_1] >>
                             *(('^' >> atom)
                               [qi::_val = ph::new_<ast::BinaryOperator>("^",qi::_val,qi::_1)]
                             | ('(' >> (expression % ',') >> ')')
                               [qi::_val = ph::new_<ast::FunctionCall>(qi::_val,qi::_1)]
                              )
                           ;

                atom       = ("(" >> expression >> ")")
                           | integer
                           | variable
                           | function
                           ;
                source     = (qi::eps >> *(statement))
                             [qi::_val = ph::new_<ast::SourceCode>(qi::_1)];


            }
            ph::function<error_handler_f> handler;
            ph::function<annotation_f<Iterator>> annotate;

            qi::rule<Iterator,ast::Expression*(),Skipper>       expression;
            qi::rule<Iterator,ast::Identifier()>                identifier;
            qi::rule<Iterator,ast::Expression*(),Skipper>       test;
            qi::rule<Iterator,ast::Expression*(),Skipper>       or_test;
            qi::rule<Iterator,ast::Expression*(),Skipper>       and_test;
            qi::rule<Iterator,ast::Expression*(),Skipper>       not_test;
            qi::rule<Iterator,ast::Expression*(),Skipper>       comparison;
            qi::rule<Iterator,ast::Expression*(),Skipper>       multi;
            qi::rule<Iterator,ast::Expression*(),Skipper>       addi;
            qi::rule<Iterator,ast::Expression*(),Skipper>       unary;
            qi::rule<Iterator,ast::Expression*(),Skipper>       power;
            qi::rule<Iterator,ast::Expression*(),Skipper>       atom;
            qi::rule<Iterator,ast::SourceCode*(),Skipper>       source;
            qi::rule<Iterator,ast::Number*()>                   integer;
            qi::rule<Iterator,ast::Function*(),Skipper>         function;
            qi::rule<Iterator,ast::Variable*()>                 variable;
            qi::rule<Iterator,ast::Definement*(),Skipper>       definement;
            qi::rule<Iterator,ast::IfElseStatement*(),Skipper>  ifelse_stmt;
            qi::rule<Iterator,ast::Statement*(),Skipper>        statement;
        };
    }
}

#endif
