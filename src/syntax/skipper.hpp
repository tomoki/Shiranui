#ifndef SKIPPER_HPP_INCLUDED
#define SKIPPER_HPP_INCLUDED

#define BOOST_SPIRIT_USE_PHOENIX_V3
#include <boost/spirit/include/qi.hpp>

namespace shiranui{
    namespace syntax{
        namespace qi = boost::spirit::qi;

        template<typename Iterator>
        struct CommentSkipper : public boost::spirit::qi::grammar<Iterator>{
            CommentSkipper() : CommentSkipper::base_type(comment,"comment"){
                namespace qi = boost::spirit::qi;
                using namespace qi::standard_wide;

                comment = space
                        | (qi::lit("//") >> *(char_ - (qi::eol | qi::eoi)) >> (qi::eol|qi::eoi))
                        ;
            }
            boost::spirit::qi::rule<Iterator> comment;
        };

    }
}
#endif
