#ifndef MSIC_HPP_INCLUDED
#define MSIC_HPP_INCLUDED

#include <iostream>
#include <vector>
#include <memory>
#include <utility>

// #define repeat(i,n) for(int i=0;i<static_cast<int>(n);i++)
#define dump(x,to)  to << #x << "=" << (x) << " (L:" << __LINE__ << ")"<< std::endl

template<typename T>
using sp = std::shared_ptr<T>;

namespace shiranui{
    template<typename T,typename IT>
    struct Indexer{
        struct iterator{
            typedef IT inner_iterator;
            typedef typename std::iterator_traits<inner_iterator>::reference inner_reference;
            struct reference : std::pair<std::size_t,inner_reference>{
                reference(std::size_t s,inner_reference i) :
                    std::pair<std::size_t,inner_reference>(s,i) {}
                const std::size_t& index = (*this).first;
                const inner_reference& value = (*this).second;
            };

            iterator(inner_iterator i) : pos(0),it(i) {}
            reference operator*() const {
                return reference(pos,*it);
            }
            iterator& operator++(){
                ++pos;++it;
                return *this;
            }
            iterator operator++(int){
                iterator tmp(*this);
                ++(*this);
                return tmp;
            }
            bool operator==(const iterator& rhs) const{
                return it == rhs.it;
            }
            bool operator!=(const iterator& rhs) const{
                return !(*this == rhs);
            }

        private:
            std::size_t pos;
            inner_iterator it;
        };


        Indexer(T& t): container(t) {};
        iterator begin() const{
            return iterator(container.begin());
        }
        iterator end() const{
            return iterator(container.end());
        }

    private:
        T& container;
    };

    template<typename T>
    Indexer<T,typename T::iterator> enumerate(T& t){
        return Indexer<T,typename T::iterator>(t);
    }
    // value will be readonly.
    template<typename T>
    Indexer<T,typename T::const_iterator> const_enumerate(T& t){
        return Indexer<T,typename T::const_iterator>(t);
    }
}

// test codes.
template<typename T>
std::ostream& operator<<(std::ostream& os,const std::vector<T>& val){
    os << "[";
    for(const auto& v : val){
        os << v << ",";
    }
    os << "]";
    return os;
}

#endif
