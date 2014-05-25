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
            Parser(Iterator first) : Parser::base_type(source_code),
                                     annotate(first){
                using namespace qi;
                keyword_let = lit("let");
                keyword_mut = lit("mut");
                keyword_if = lit("if");
                keyword_else = lit("else");
                keyword_arrow = lit("->");
                semicolon = lit(";");
                one_equal = lit("=");
                keyword_lambda = lit("\\");

                identifier = as_string[(alpha | char_('_')) >> *(alnum | char_('_'))];
                number = int_ [qi::_val = ph::new_<ast::Number>(qi::_1)];
                string = lexeme[lit("\"") > *(char_ - "\"") > lit("\"")]
                              [qi::_val = ph::new_<ast::String>(qi::_1)];

                variable = identifier 
                    [qi::_val = ph::new_<ast::Variable>(qi::_1)];

                function = (keyword_lambda >> '(' >> (identifier % ',') >> ')'
                                          >> '{' >> *statement >> '}')
                           [qi::_val = ph::new_<ast::Function>(qi::_1,qi::_2)];

                function_call = (identifier >> '(' >> (expression % ',') >> ')')
                           [qi::_val = ph::new_<ast::FunctionCall>(qi::_1,qi::_2)];

                // doesn't work.
                if_else_expression = (keyword_if >> expression >> '{' >> *statement >> '}'
                                                >> keyword_else >> '{' >> *statement >> '}')
                          [qi::_val = ph::new_<ast::IfElseExpression>(qi::_1,qi::_2,qi::_3)];

                // DONOT USE > and >> together.
                const_definement = (keyword_let > identifier > one_equal > expression > semicolon)
                                   [qi::_val = ph::new_<ast::ConstDefinement>(qi::_1,qi::_2)];
                var_definement   = (keyword_mut > identifier > one_equal > expression > semicolon)
                                   [qi::_val = ph::new_<ast::VarDefinement>(qi::_1,qi::_2)];
                if_statement = (keyword_if >> expression >> '{' >> *statement >> '}')
                                   [qi::_val = ph::new_<ast::IfStatement>(qi::_1,qi::_2)];

                statement = (
                             const_definement  |
                             var_definement    |
                             if_statement
                             );
                expression = (function_call |
                              number        |
                              string        |
                              function      |
                              variable      |
                              if_else_expression
                              );
                // what qi::eps for?
                source_code = (qi::eps >> *(statement))
                              [qi::_val = ph::new_<ast::SourceCode>(qi::_1)];

                on_error<fail>(var_definement, handler(_1, _2, _3, _4));
                on_error<fail>(const_definement, handler(_1, _2, _3, _4));
                on_error<fail>(if_statement, handler(_1, _2, _3, _4));
                on_error<fail>(source_code, handler(_1, _2, _3, _4));

                auto set_location_info = annotate(_val,_1,_3);

                // cause bug!!
                on_success(identifier,set_location_info);
                on_success(number,set_location_info);
                on_success(string,set_location_info);
                on_success(function,set_location_info);

                on_success(function_call,set_location_info);
                on_success(var_definement,set_location_info);
                on_success(const_definement,set_location_info);
                on_success(if_statement,set_location_info);

//                 // cause error.
//                 // on_success(expression,set_location_info);
//                 // on_success(statement,set_location_info);
//                 on_success(source_code,set_location_info);
// 
                identifier.name("identifier");
                number.name("number");
                string.name("string");
                variable.name("variable");
                function.name("function");
                function_call.name("function_call");
                var_definement.name("mut_defiment");
                const_definement.name("const_defiment");
                if_statement.name("if_statement");
                expression.name("expression");
                statement.name("statement");

                BOOST_SPIRIT_DEBUG_NODES((keyword_let)(keyword_mut)(keyword_arrow)(source_code)
                                         (semicolon)(statement)
                                         (identifier)(var_definement)(const_definement)
                                         (if_statement));
            }
            ph::function<error_handler_f> handler;
            ph::function<annotation_f<Iterator>> annotate;

            // meta
            qi::rule<Iterator,ast::Identifier()> identifier;
            // immidiate
            qi::rule<Iterator,ast::Number*()> number;
            qi::rule<Iterator,ast::String*()> string;
            qi::rule<Iterator,ast::Variable*()> variable;
            qi::rule<Iterator,ast::Function*(),Skipper> function;

            // expression
            qi::rule<Iterator,ast::FunctionCall*(),Skipper> function_call;
            qi::rule<Iterator,ast::IfElseExpression*(),Skipper> if_else_expression;

            // statements
            qi::rule<Iterator,ast::VarDefinement*(),Skipper> var_definement;
            qi::rule<Iterator,ast::ConstDefinement*(),Skipper> const_definement;
            qi::rule<Iterator,ast::IfStatement*(),Skipper> if_statement;

            qi::rule<Iterator,ast::Expression*(),Skipper> expression;
            qi::rule<Iterator,ast::Statement*(),Skipper> statement;
            qi::rule<Iterator,ast::SourceCode*(),Skipper> source_code;

            qi::rule<Iterator> keyword_let;
            qi::rule<Iterator> keyword_mut;
            qi::rule<Iterator> keyword_arrow;
            qi::rule<Iterator> keyword_lambda;
            qi::rule<Iterator> keyword_if;
            qi::rule<Iterator> keyword_else;

            qi::rule<Iterator> one_equal;
            qi::rule<Iterator> semicolon;
        };
    }
}

#endif
