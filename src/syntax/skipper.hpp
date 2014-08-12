#ifndef SKIPPER_HPP_INCLUDED
#define SKIPPER_HPP_INCLUDED

#define BOOST_SPIRIT_USE_PHOENIX_V3
#include <boost/spirit/include/qi.hpp>

namespace shiranui{
    namespace syntax{
        namespace qi = boost::spirit::qi;
        namespace ascii = boost::spirit::ascii;


        template<typename Iterator>
        struct CommentSkipper : public boost::spirit::qi::grammar<Iterator>{
            CommentSkipper() : CommentSkipper::base_type(comment,"comment"){
                namespace qi = boost::spirit::qi;
                namespace ascii = boost::spirit::qi::ascii;

                comment = ascii::space
                        | (qi::lit("//") >> *(qi::char_ - qi::eol) >> qi::eol)
                        ;
            }
            boost::spirit::qi::rule<Iterator> comment;
        };

    }
}
#endif
