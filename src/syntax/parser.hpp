#ifndef PARSER_HPP_INCLUDED
#define PARSER_HPP_INCLUDED
#define BOOST_SPIRIT_UNICODE
#define BOOST_SPIRIT_USE_PHOENIX_V3
#include <boost/spirit/include/qi.hpp>

#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_stl.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_fusion.hpp>
#include <boost/spirit/repository/include/qi_iter_pos.hpp>

#include <boost/fusion/include/io.hpp>
#include <boost/fusion/include/std_pair.hpp>

#include <iostream>
#include <iomanip>
#include <string>

#include "ast.hpp"
#include "lambda_man.hpp"
#include "../misc.hpp"
#include "../point_iterator.hpp"
#include "skipper.hpp"

// to put line,column info to AST,see following url.
// http://stackoverflow.com/questions/19612657/boostspirit-access-position-iterator-from-semantic-actions
namespace shiranui{
    namespace syntax{
        namespace qi = boost::spirit::qi;
        typedef point_iterator<std::string::const_iterator> pos_iterator_t;

        template<typename T>
        struct qi_make_shared_struct{
            template<typename... Args>
            sp<T> operator()(Args&&... args) const{
                return std::make_shared<T>(std::forward<Args>(args)...);
            }
        };
        template<typename T,typename... Args>
        auto qi_make_shared(Args&&... args)
            -> decltype(boost::phoenix::function<qi_make_shared_struct<T>>()(std::forward<Args>(args)...)){
            return boost::phoenix::function<qi_make_shared_struct<T>>()(std::forward<Args>(args)...);
        }

        template<typename Iterator>
        struct annotation_f{
            typedef void result_type;
            annotation_f(){}
            template<typename Val,typename First,typename Last>
            void operator()(Val& v,First f,Last l) const{
                do_annotate(v,f,l);
            }
        private:
            void static do_annotate(ast::LocationInfo& li,Iterator f,Iterator l){
                li.point = get_point(f);
                li.length = std::distance(f,l);
            }
            void static do_annotate(sp<ast::LocationInfo> li,Iterator f,Iterator l){
                li->point = get_point(f);
                li->length = std::distance(f,l);
            }

            static void do_annotate(...){
                std::cerr << "(not having LocationInfo)" << std::endl;
            }
        };
        template<typename Iterator>
        struct scan_for_lambda_exp{
            scan_for_lambda_exp(){}
            template<typename Val,typename First,typename Last>
            void operator()(Val& v,First f,Last l) const{
                do_annotate(v,f,l);
            }
        private:
            void static do_annotate(sp<ast::SourceCode> sc,Iterator f,Iterator l){
                return do_annotate(*sc,f,l);
            }
            void static do_annotate(ast::SourceCode& sc,Iterator,Iterator){
                auto pair_of_from_block_and_from_marker = use_LambdaMan(sc);
                sc.where_is_function_from = pair_of_from_block_and_from_marker.first;
                sc.marker_to_lambda = pair_of_from_block_and_from_marker.second;
            }
        };
        template<typename Iterator=pos_iterator_t,typename Skipper=CommentSkipper<Iterator>>
        struct Parser : public boost::spirit::qi::grammar<Iterator,sp<ast::SourceCode>()
                      ,Skipper>{
            using skip_type = Skipper;
            template<typename T>
            using rule_no_skip = qi::rule<Iterator,T>;
            template<typename T>
            using rule_with_skip = qi::rule<Iterator,T,Skipper>;
            template<typename T>
            using rule_with_local_for_binary = qi::rule<Iterator,boost::spirit::locals<Iterator>,T,Skipper>;
            // change const_definement to sourcecode.
            Parser() : Parser::base_type(source),
                       annotate(){
                namespace ph = boost::phoenix;

                using boost::spirit::repository::qi::iter_pos;
                //using namespace qi::ascii;
                using namespace qi::standard_wide;
                using qi::lit;
                auto set_location_info = annotate(qi::_val,qi::_1,qi::_3);
                auto scan_lambda_info = scan_lambda(qi::_val,qi::_1,qi::_3);
                {
                    identifier.name("identifier");
                    identifier = boost::spirit::as_string[(alpha >> *(alnum | char_('_')))];
                    on_success(identifier,set_location_info);
                }

                {
                    integer.name("integer");
                    integer    = qi::int_ [qi::_val = qi_make_shared<ast::Number>(qi::_1)];
                    on_success(integer,set_location_info);
                }

                {
                    string.name("string");
                    string = lit("\"") > 
                               boost::spirit::as_string[*(char_ - "\"")]
                                [qi::_val = qi_make_shared<ast::String>(qi::_1)]
                           > lit("\"");

                    on_success(string,set_location_info);
                }

                {
                    array.name("array");
                    array = (qi::lexeme["["] >> "]")
                             [qi::_val = qi_make_shared<ast::Enum>()]
                          | ("[" >> (expression % ",") >> "]")
                             [qi::_val = qi_make_shared<ast::Enum>(qi::_1)]
                          | ("[" >> expression >> ".." >> expression >> "]")
                             [qi::_val = qi_make_shared<ast::Interval>(qi::_1,nullptr,qi::_2,true)]
                          | ("[" >> expression >> "," >> expression >> ".." >> expression >> "]")
                             [qi::_val = qi_make_shared<ast::Interval>(qi::_1,qi::_2,qi::_3,true)]
                          | ("[" >> expression >> ".." >> expression >> ")")
                             [qi::_val = qi_make_shared<ast::Interval>(qi::_1,nullptr,qi::_2,false)]
                          | ("[" >> expression >> "," >> expression >> ".." >> expression >> ")")
                             [qi::_val = qi_make_shared<ast::Interval>(qi::_1,qi::_2,qi::_3,false)]

                          ;
                    on_success(array,set_location_info);
                }
                {
                    variable.name("variable");
                    variable   = identifier [qi::_val = qi_make_shared<ast::Variable>(qi::_1)];
                    on_success(variable,set_location_info);
                }
                {
                    function.name("function");
                    function  = (lit("\\") >> identifier >> "(" >> (identifier % ",") >> ")" >> block)
                                  [qi::_val = qi_make_shared<ast::Function>(qi::_1,qi::_2,qi::_3)]
                               | (lit("\\") >> identifier >> "(" >> ")" >> block)
                                  [qi::_val = qi_make_shared<ast::Function>(qi::_1,std::vector<ast::Identifier>(),qi::_2)]
                               | (lit("\\") >> "(" >> (identifier % ",") >> ")" >> block)
                                  [qi::_val = qi_make_shared<ast::Function>(qi::_1,qi::_2)]
                               | (lit("\\") > "(" > ")" > block)
                                  [qi::_val = qi_make_shared<ast::Function>(std::vector<ast::Identifier>(),qi::_1)]
                               ;
                    on_success(function,set_location_info);

                }

                {
                    definement.name("definement");
                    definement = ("let" > identifier > "=" > expression)
                                  [qi::_val = qi_make_shared<ast::Definement>(qi::_1,qi::_2,true)]
                               | ("mut" > identifier > "=" > expression)
                                  [qi::_val = qi_make_shared<ast::Definement>(qi::_1,qi::_2,false)]
                               ;
                    on_success(definement,set_location_info);
                }
                {
                    expression_statement.name("expression_statement");
                    expression_statement = (qi::eps >> expression)
                                           [qi::_val = qi_make_shared<ast::ExpressionStatement>(qi::_1)]
                                         ;
                    on_success(expression_statement,set_location_info);
                }
                {
                    ifelse_stmt.name("if_else_statement");
                    ifelse_stmt= ("if" >> expression >> block >> "else" >> block)
                                  [qi::_val = qi_make_shared<ast::IfElseStatement>(qi::_1,qi::_2,qi::_3)]
                               | ("if" >> expression >> block)
                                  [qi::_val = qi_make_shared<ast::IfElseStatement>(qi::_1,qi::_2)]
                               ;
                    on_success(ifelse_stmt,set_location_info);
                }
                {
                    for_stmt.name("for_statement");
                    for_stmt = ("for" > identifier > "in" > expression > block)
                                [qi::_val = qi_make_shared<ast::ForStatement>(qi::_1,qi::_2,qi::_3)];
                    on_success(for_stmt,set_location_info);
                }
                {
                    return_stmt.name("return_statement");
                    return_stmt = ("return" > expression)
                                 [qi::_val = qi_make_shared<ast::ReturnStatement>(qi::_1)]
                                ;
                    on_success(return_stmt,set_location_info);
                }
                {
                    probe_stmt.name("probe_statement");
                    probe_stmt = ("probe" > expression)
                        [qi::_val = qi_make_shared<ast::ProbeStatement>(qi::_1)]
                                ;
                    on_success(probe_stmt,set_location_info);
                }
                {
                    assert_stmt.name("assert_statement");
                    assert_stmt = ("assert" > expression)
                                 [qi::_val = qi_make_shared<ast::AssertStatement>(qi::_1)]
                                ;
                    on_success(assert_stmt,set_location_info);
                }
                {
                    block.name("block");
                    block = (lit("{") [qi::_val = qi_make_shared<ast::Block>()])
                        > *(statement
                           [ph::bind(&ast::Block::add_statement,*qi::_val,qi::_1)]
                         | (lit("pre") > block)
                           [ph::bind(&ast::Block::add_pre,*qi::_val,qi::_1)]
                         | (lit("post") >> '(' >> identifier >> ')' >> block)
                           [ph::bind(&ast::Block::add_post,*qi::_val,qi::_1,qi::_2)]
                         | (lit("post") >> block)
                           [ph::bind(&ast::Block::add_post,*qi::_val,ast::Identifier(""),qi::_1)]
                         | (lit("invariant") > block)
                           [ph::bind(&ast::Block::add_invariant,*qi::_val,qi::_1)]
                         | flymark
                           [ph::bind(&ast::Block::add_flymark,*qi::_val,qi::_1)]
                        ) > lit("}");
                    on_success(block,set_location_info);
                }
                {
                    assignment.name("assignment");
                    assignment = (identifier >> "<-" >> expression)
                                  [qi::_val = qi_make_shared<ast::Assignment>(qi::_1,qi::_2)]
                               ;
                    on_success(assignment,set_location_info);
                }
                {
                    statement.name("statement");
                    statement  = (definement > ";")
                               | (assignment > ";")
                               | ifelse_stmt
                               | for_stmt
                               | block
                               | (return_stmt > ";")
                               | (assert_stmt > ";")
                               | (probe_stmt > ";")
                               | (expression_statement >> ";")
                               ;
                    on_success(statement,set_location_info);
                }

                {
                    expression.name("expression");
                    expression = ifelse_expr [qi::_val = qi::_1]
                               | test [qi::_val = qi::_1]
                               ;
                    on_success(expression,set_location_info);
                }

                {
                    ifelse_expr.name("if_else_expression");
                    ifelse_expr= ("if" >> expression >> "then"
                                       >> expression >> "else" >> expression)
                                 [qi::_val = qi_make_shared<ast::IfElseExpression>(qi::_1,qi::_2,qi::_3)];
                    on_success(ifelse_expr,set_location_info);
                }
                {
                    test.name("test");
                    test       = or_test [qi::_val = qi::_1];
                    on_success(test,set_location_info);
                }
                {
                    or_test.name("or_test");
                    or_test    = (iter_pos >> and_test) [qi::_val = qi::_2,qi::_a = qi::_1] >>
                                  *(("or" >> and_test >> iter_pos)
                                   [qi::_val = qi_make_shared<ast::BinaryOperator>("or",qi::_val,qi::_1),annotate(qi::_val,qi::_a,qi::_2)])
                               ;
                    on_success(or_test,set_location_info);
                }
                {
                    and_test.name("and_test");
                    and_test   = (iter_pos >> not_test) [qi::_val = qi::_2,qi::_a = qi::_1] >> 
                                 *(("and" >> not_test >> iter_pos)
                                   [qi::_val = qi_make_shared<ast::BinaryOperator>("and",qi::_val,qi::_1),annotate(qi::_val,qi::_a,qi::_2)])
                               ;
                    on_success(and_test,set_location_info);
                }
                {
                    not_test.name("not_test");
                    not_test   = ("not" >> not_test)
                                  [qi::_val = qi_make_shared<ast::UnaryOperator>("not",qi::_1)]
                               | comparison [qi::_val = qi::_1]
                               ;
                    on_success(not_test,set_location_info);
                }

                {
                    comparison.name("comparison");
                    comparison = (iter_pos >> addi) [qi::_val = qi::_2,qi::_a = qi::_1] >> 
                                 *(("=" >> addi >> iter_pos)
                                   [qi::_val = qi_make_shared<ast::BinaryOperator>("=",qi::_val,qi::_1),annotate(qi::_val,qi::_a,qi::_2)]
                                 | ("/=" >> addi >> iter_pos)
                                   [qi::_val = qi_make_shared<ast::BinaryOperator>("/=",qi::_val,qi::_1),annotate(qi::_val,qi::_a,qi::_2)]
                                 | ("<" >> addi >> iter_pos)
                                   [qi::_val = qi_make_shared<ast::BinaryOperator>("<", qi::_val,qi::_1),annotate(qi::_val,qi::_a,qi::_2)]
                                 | (">" >> addi >> iter_pos)
                                   [qi::_val = qi_make_shared<ast::BinaryOperator>(">", qi::_val,qi::_1),annotate(qi::_val,qi::_a,qi::_2)]
                                 | (">=" >> addi >> iter_pos)
                                   [qi::_val = qi_make_shared<ast::BinaryOperator>(">=",qi::_val,qi::_1),annotate(qi::_val,qi::_a,qi::_2)]
                                 | ("<=" >> addi >> iter_pos)
                                   [qi::_val = qi_make_shared<ast::BinaryOperator>("<=",qi::_val,qi::_1),annotate(qi::_val,qi::_a,qi::_2)]
                                 )
                               ;
                    on_success(comparison,set_location_info);
                }

                {
                    addi.name("addi");
                    addi       = (iter_pos >> multi) [qi::_val = qi::_2,qi::_a = qi::_1] >> 
                                 *(('+' >> multi >> iter_pos)
                                   [qi::_val = qi_make_shared<ast::BinaryOperator>("+",qi::_val,qi::_1),annotate(qi::_val,qi::_a,qi::_2)]
                                 | ('-' >> multi >> iter_pos)
                                   [qi::_val = qi_make_shared<ast::BinaryOperator>("-",qi::_val,qi::_1),annotate(qi::_val,qi::_a,qi::_2)]
                                  )
                               ;
                    on_success(addi,set_location_info);
                }

                {
                    multi.name("multi");
                    multi      = (iter_pos >> unary) [qi::_val = qi::_2,qi::_a = qi::_1] >>
                                 *(('*' >> unary >> iter_pos)
                                   [qi::_val = qi_make_shared<ast::BinaryOperator>("*",qi::_val,qi::_1),annotate(qi::_val,qi::_a,qi::_2)]
                                 | ('/' >> unary >> iter_pos)
                                   [qi::_val = qi_make_shared<ast::BinaryOperator>("/",qi::_val,qi::_1),annotate(qi::_val,qi::_a,qi::_2)]
                                 | ('%' >> unary >> iter_pos)
                                   [qi::_val = qi_make_shared<ast::BinaryOperator>("%",qi::_val,qi::_1),annotate(qi::_val,qi::_a,qi::_2)]
                                  )
                               ;
                    on_success(multi,set_location_info);
                }
                {
                    unary.name("unary");
                    unary      = power [qi::_val = qi::_1]
                               | ("+" >> power)
                                  [qi::_val = qi_make_shared<ast::UnaryOperator>("+",qi::_1)]
                               | ("-" >> power)
                                  [qi::_val = qi_make_shared<ast::UnaryOperator>("-",qi::_1)]
                               ;
                    on_success(unary,set_location_info);
                }
                {
                    power.name("power");
                    power      = (iter_pos >> atom) [qi::_val = qi::_2,qi::_a = qi::_1] >>
                                 *(('^' >> atom >> iter_pos)
                                   [qi::_val = qi_make_shared<ast::BinaryOperator>("^",qi::_val,qi::_1),annotate(qi::_val,qi::_a,qi::_2)]
                                 | ('(' >> (expression % ',') >> ')' >> iter_pos)
                                   [qi::_val = qi_make_shared<ast::FunctionCall>(qi::_val,qi::_1),annotate(qi::_val,qi::_a,qi::_2)]
                                 | (lit('(') >> ')' >> iter_pos)
                                   [qi::_val = qi_make_shared<ast::FunctionCall>(
                                                        qi::_val,std::vector<sp<ast::Expression>>()),annotate(qi::_val,qi::_a,qi::_1)]
                                  )
                               ;
                    on_success(power,set_location_info);
                }

                {
                    atom.name("atom");
                    atom       = ("(" > expression > ")")
                               | integer
                               | string
                               | array
                               | variable
                               | function
                               | data_dsl
                               ;
                    on_success(atom,set_location_info);
                }
                {
                    // #+ sum(1) -> ;
                    // #+ sum(1) -> 1;
                    // #+ sum(-1) -> "error";
                    // #- sum(1) -> 1;
                    // #- sum(1) -> 2 || 1;
                    // #- sum(-1) -> 1 || "assert_error";

                    // change expression to expression or error.
                    // add eol or something like that.
                    // do not use no_skip contains expression
                    flyline.name("flyline");
                    flyline = ("#-" >> expression >> "->" >> expression >> "||" >> expression >> ";")
                               [qi::_val = qi_make_shared<ast::TestFlyLine>(qi::_1,qi::_2,qi::_3)]
                            | ("#-" >> expression >> "->" >> expression >> ";")
                               [qi::_val = qi_make_shared<ast::TestFlyLine>(qi::_1,qi::_2,nullptr)]
                            | ("#+" >> expression >> "->" >> expression >> ";")
                               [qi::_val = qi_make_shared<ast::IdleFlyLine>(qi::_1,qi::_2)]
                            | ("#+" > expression > "->" > ";")
                               [qi::_val = qi_make_shared<ast::IdleFlyLine>(qi::_1,nullptr)]
                            ;
                    on_success(flyline,set_location_info);
                }

                {
                    flymark.name("flymark");
                    flymark = ("#*" >> expression >> "->" >> ";")
                                [qi::_val = qi_make_shared<ast::FlyMark>(qi::_1)]
                            | ("#*" >> expression >> "->" >> (expression % ",") >> ";")
                                [qi::_val = qi_make_shared<ast::FlyMark>(qi::_1,qi::_2)]
                            ;
                    on_success(flymark,set_location_info);
                }
                {
                    source.name("source");
                    source = qi::eps [qi::_val = qi_make_shared<ast::SourceCode>()]
                          > *(statement
                                [ph::bind(&ast::SourceCode::add_statement,*qi::_val,qi::_1)]
                              |flyline
                                [ph::bind(&ast::SourceCode::add_flyline,*qi::_val,qi::_1)]
                              )
                           ;
                    on_success(source,set_location_info);
                    on_success(source,scan_lambda_info);
                }
                // DSL section
                {
                    data_dsl.name("dsl for data");
                    data_dsl = ("<|" > dsl_exp > "|>")
                               [qi::_val = qi_make_shared<ast::DSL::DataDSL>(qi::_1)];
                    on_success(data_dsl,set_location_info);

                    dsl_exp.name("dsl exp");
                    dsl_exp = dsl_immediate
                            | dsl_define_var
                            | dsl_var
                            ;
                    on_success(dsl_exp,set_location_info);

                    dsl_immediate.name("dsl immediate");
                    dsl_immediate = dsl_number
                                  | dsl_string
                                  | dsl_array
                                  | dsl_function
                                  ;
                    on_success(dsl_immediate,set_location_info);

                    dsl_number.name("dsl name");
                    dsl_number = qi::int_ [qi::_val = qi_make_shared<ast::DSL::DSLInteger>(qi::_1)];
                    on_success(dsl_number,set_location_info);

                    dsl_string.name("dsl name");
                    dsl_string = lit("\"") >
                               boost::spirit::as_string[*(char_ - "\"")]
                                [qi::_val = qi_make_shared<ast::DSL::DSLString>(qi::_1)]
                               > lit("\"");
                    on_success(dsl_string,set_location_info);

                    dsl_var.name("dsl var");
                    dsl_var = boost::spirit::as_string[(alpha >> *(alnum | char_('_')))]
                             [qi::_val = qi_make_shared<ast::DSL::DSLVariable>(qi::_1)];
                    on_success(dsl_var,set_location_info);

                    dsl_define_var.name("dsl define var");
                    dsl_define_var = (dsl_var >> "=" >> dsl_immediate)
                                     [qi::_val = qi_make_shared<ast::DSL::DSLDefine>(qi::_1,qi::_2)];
                    on_success(dsl_define_var,set_location_info);

                    dsl_array.name("dsl array");
                    dsl_array = (qi::lexeme["["] >> "]")
                                 [qi::_val = qi_make_shared<ast::DSL::DSLArray>()]
                              | ("[" >> (dsl_exp % ",") >> "]")
                                [qi::_val = qi_make_shared<ast::DSL::DSLArray>(qi::_1)];
                    on_success(dsl_array,set_location_info);

                    dsl_env_bind.name("dsl env bind");
                    dsl_env_bind = (dsl_var >> "=" >> dsl_exp);
                                     // [qi::_val = std::make_pair(qi::_1,qi::_2)];
                    dsl_function = (qi::lexeme["$"] >> "(" >> ")" >> dsl_var)
                                     [qi::_val = qi_make_shared<ast::DSL::DSLFunction>(
                                                                  std::vector<std::pair<sp<ast::DSL::DSLVariable>,
                                                                                        sp<ast::DSL::DSLInner> > >(),qi::_1)]
                                 | (qi::lexeme["$"] >> "(" >> (dsl_env_bind % ",") >> ")" >> dsl_var)
                                     [qi::_val = qi_make_shared<ast::DSL::DSLFunction>(qi::_1,qi::_2)]
                                 ;
                }
            }

            //boost::phoenix::function<error_handler_f> handler;
            boost::phoenix::function<annotation_f<Iterator>> annotate;
            boost::phoenix::function<scan_for_lambda_exp<Iterator>> scan_lambda;

            rule_no_skip<ast::Identifier()> identifier;
            rule_with_skip<sp<ast::Expression>()> expression;
            rule_with_skip<sp<ast::Expression>()> test;

            rule_with_local_for_binary<sp<ast::Expression>()> and_test;
            rule_with_local_for_binary<sp<ast::Expression>()> or_test;
            rule_with_local_for_binary<sp<ast::Expression>()> comparison;
            rule_with_local_for_binary<sp<ast::Expression>()> multi;
            rule_with_local_for_binary<sp<ast::Expression>()> addi;
            rule_with_local_for_binary<sp<ast::Expression>()> power;
            rule_with_skip<sp<ast::Expression>()> not_test;
            rule_with_skip<sp<ast::Expression>()> unary;
            rule_with_skip<sp<ast::Expression>()> atom;
            rule_with_skip<sp<ast::SourceCode>()> source;

            rule_no_skip<sp<ast::Number>()> integer;
            rule_no_skip<sp<ast::String>()> string;
            rule_no_skip<sp<ast::Variable>()> variable;

            rule_with_skip<sp<ast::Array>()> array;
            rule_with_skip<sp<ast::Function>()> function;
            rule_with_skip<sp<ast::Definement>()> definement;
            rule_with_skip<sp<ast::ExpressionStatement>()> expression_statement;
            rule_with_skip<sp<ast::IfElseStatement>()>  ifelse_stmt;
            rule_with_skip<sp<ast::ForStatement>()> for_stmt;
            rule_with_skip<sp<ast::ReturnStatement>()> return_stmt;
            rule_with_skip<sp<ast::ProbeStatement>()> probe_stmt;
            rule_with_skip<sp<ast::AssertStatement>()> assert_stmt;

            rule_with_skip<sp<ast::IfElseExpression>()> ifelse_expr;
            rule_with_skip<sp<ast::Block>()> block;
            rule_with_skip<sp<ast::Assignment>()> assignment;
            rule_with_skip<sp<ast::FlyLine>()> flyline;
            rule_with_skip<sp<ast::FlyMark>()> flymark;

            rule_with_skip<sp<ast::Statement>()> statement;

            // DSL
            rule_with_skip<sp<ast::DSL::DataDSL>()> data_dsl;
            rule_with_skip<sp<ast::DSL::DSLInner>()> dsl_exp;
            rule_with_skip<sp<ast::DSL::DSLImmediate>()> dsl_immediate;
            rule_with_skip<sp<ast::DSL::DSLDefine>()> dsl_define_var;
            rule_no_skip<sp<ast::DSL::DSLVariable>()> dsl_var;
            rule_no_skip<sp<ast::DSL::DSLInteger>()> dsl_number;
            rule_no_skip<sp<ast::DSL::DSLString>()> dsl_string;
            rule_with_skip<sp<ast::DSL::DSLArray>()> dsl_array;
            rule_with_skip<std::pair<sp<ast::DSL::DSLVariable>,
                                     sp<ast::DSL::DSLInner> >() > dsl_env_bind;
            rule_with_skip<sp<ast::DSL::DSLFunction>() > dsl_function;
        };
        template<typename IT>
        bool parse(IT& iter,IT& last,Parser<IT>& resolver,sp<ast::SourceCode>& program){
            typename Parser<IT>::skip_type skipper;
            return boost::spirit::qi::phrase_parse(iter,last,resolver,skipper,program);
        }
    }
}

#endif
