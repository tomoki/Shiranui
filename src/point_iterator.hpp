/*==============================================================================
    Copyright (c) 2001-2011 Joel de Guzman
    Copyright (c) 2010      Bryce Lelbach
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

        std::size_t position() const;
        std::size_t position_point() const;

    private:
        friend class boost::iterator_core_access;

        void increment();

        std::size_t line; // The line position.
        std::size_t point;
        typename std::iterator_traits<Iterator>::value_type prev;
    };
    //]

    template <class Iterator>
    point_iterator<Iterator>::point_iterator() :
        point_iterator::iterator_adaptor_(), line(1), point(1), prev(0) { }

    template <class Iterator>
    point_iterator<Iterator>::point_iterator(Iterator base) :
        point_iterator::iterator_adaptor_(base), line(1), point(1), prev(0) { }

    template <class Iterator>
    std::size_t point_iterator<Iterator>::position() const
    {
        return line;
    }
    template <class Iterator>
    std::size_t point_iterator<Iterator>::position_point() const
    {
        return point;
    }

    template<class Iterator>
    void point_iterator<Iterator>::increment()
    {
        typename std::iterator_traits<Iterator>::reference
          ref = *(this->base());

        switch (ref) {
          case '\r':
            if (prev != '\n')
              ++line;
            break;
          case '\n':
            if (prev != '\r')
              ++line;
            break;
          default:
            break;
        }
        point++;
        prev = ref;
        ++this->base_reference();
    }

    template <class Iterator>
    inline std::size_t get_line(Iterator);

    template <class Iterator>
    inline Iterator get_line_start(Iterator lower_bound, Iterator current); 

    template <class Iterator>
    inline boost::iterator_range<Iterator>
    get_current_line(Iterator lower_bound, Iterator current,
                     Iterator upper_bound); 
    template <class Iterator>
    inline std::size_t get_column(Iterator lower_bound, Iterator current,
                                  std::size_t tabs = 4);

    template <class Iterator>
    inline std::size_t get_line(Iterator)
    {
        return -1;
    }
    
    template <class Iterator>
    inline std::size_t get_line(point_iterator<Iterator> i)
    {
        return i.position();
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
    
    template <class Iterator>
    inline Iterator get_line_start(Iterator lower_bound, Iterator current)
    {
        // cover LF,CR+LF,CR,LF+RF.
        // but if *current == '\r' or *current == '\n',
        //          result will be something worng.
        Iterator latest = lower_bound;
        bool prev_was_newline = false;
        for (Iterator i = lower_bound; i != current; ++i) {
            if (prev_was_newline) {
                latest = i;
            }
            prev_was_newline = (*i == '\r') || (*i == '\n');
        }
        if (prev_was_newline) {
            latest = current;
        }
        return latest;
    }

    template <class Iterator>
    inline Iterator get_line_end(Iterator current, Iterator upper_bound)
    {
        // if current is at '\r' or '\n',may return something unexpected.
        for (Iterator i = current; i != upper_bound; ++i) {
            if ((*i == '\n') || (*i == '\r')) {
                return i;
            }
        }
        return upper_bound;
    }

    
    template <class Iterator>
    inline boost::iterator_range<Iterator>
    get_current_line(Iterator lower_bound,
                     Iterator current,
                     Iterator upper_bound)
    {
        // if *current is '\r' or '\n', result will something unexpected.
        Iterator first = get_line_start(lower_bound, current);
        Iterator last = get_line_end(current, upper_bound);
        return boost::iterator_range<Iterator>(first, last);
    }
    
    template <class Iterator>
    inline std::size_t get_column(Iterator lower_bound,
                                  Iterator current,
                                  std::size_t tabs)
    {
        std::size_t column = 1;
        Iterator first = get_line_start(lower_bound, current);
      
        for (Iterator i = first; i != current; ++i) {
          switch (*i) {
            case '\t':
              column += tabs - (column - 1) % tabs;
              break;
            default:
              ++column;
          }
        }
      
        return column;
    }

}

#endif // POINT_ITERATOR_HPP_INCLUDED

