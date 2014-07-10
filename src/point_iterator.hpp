/*==============================================================================
    Copyright (c) 2014      Tomoki Imai

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
==============================================================================*/

#ifndef POINT_ITERATOR_HPP_INCLUDED
#define POINT_ITERATOR_HPP_INCLUDED

#include <boost/iterator/iterator_adaptor.hpp>
#include <boost/range/iterator_range.hpp>

namespace shiranui{
    template <class Iterator>
    class point_iterator : public boost::iterator_adaptor<
        point_iterator<Iterator>  // Derived
      , Iterator                     // Base
      , boost::use_default           // Value
      , boost::forward_traversal_tag // CategoryOrTraversal
    > {
    public:
        point_iterator();

        explicit point_iterator(Iterator);

        std::size_t position_point() const;

    private:
        friend class boost::iterator_core_access;

        void increment();

        std::size_t point;
        //typename std::iterator_traits<Iterator>::value_type prev;
    };
    //]

    template <class Iterator>
    point_iterator<Iterator>::point_iterator() :
        point_iterator::iterator_adaptor_(),  point(1){ }

    template <class Iterator>
    point_iterator<Iterator>::point_iterator(Iterator base) :
        point_iterator::iterator_adaptor_(base),point(1) { }

    template <class Iterator>
    std::size_t point_iterator<Iterator>::position_point() const
    {
        return point;
    }

    template<class Iterator>
    void point_iterator<Iterator>::increment()

    {
        point++;
        ++this->base_reference();
    }

    template <class Iterator>
    inline std::size_t get_point(Iterator)
    {
        return -1;
    }
    template <class Iterator>
    inline std::size_t get_point(point_iterator<Iterator> i)
    {
        return i.position_point();
    }
}

#endif // POINT_ITERATOR_HPP_INCLUDED

