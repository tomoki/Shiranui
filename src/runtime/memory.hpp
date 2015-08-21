#ifndef MEMORY_HPP_INCLUDED
#define MEMORY_HPP_INCLUDED

#include <cstdlib>
#include <iostream>
#include <exception>
#include <functional>
#include <boost/thread/thread.hpp>

namespace shiranui {
    namespace runtime {
        const size_t SIZE_PER_INTERPRETER = 268435456;
        // this class is not thread safe.
        struct MemoryLimitException : std::runtime_error {
            explicit MemoryLimitException(const std::string& s) : std::runtime_error(s) {}
            std::string str(){
                return "\"Memory Limit Exceeded\"";
            };
        };
        struct Memory {
            size_t size;
            bool usable;
            Memory(size_t size_)
                : size(size_), usable(true), index(0){
                memory = malloc(size);
            }
            ~Memory(){
                free(memory);
            }
            void seek(){
                index = 0;
            }
            void destruct_all_in_thread(){
                usable = false;
                boost::thread f(boost::bind(&Memory::destruct_all, this));
            }
            void destruct_all(){
                usable = false;
                for(auto f : destructor){
                    f();
                }
                destructor.clear();
                usable = true;
            }
            size_t used(){
                return index;
            }

            template<class T, class... Types>
            T* create(Types... args){
                size_t new_index = index + sizeof(T);
                if(new_index > size){
                    throw MemoryLimitException("");
                }
                T* ret = new((char*)memory+index) T(args...);
                index = new_index;
                destructor.push_back([ret](){
                        ret->~T();
                    });
                return ret;
            }
        private:
            void*  memory;
            size_t index;
            std::vector<std::function<void()>> destructor;
        };
        struct MemoryManager {
            std::vector<Memory*> chunks;
            MemoryManager(){
            }
            ~MemoryManager(){
                for(Memory* m : chunks){
                    m->destruct_all();
                    free(m);
                }
                chunks.clear();
            }
            void reserve(size_t n){
                int current = 0;
                for(Memory* m : chunks){
                    current += m->usable;
                }
                for(int i=0;i<(int)n-current;i++){
                    chunks.push_back(new Memory(SIZE_PER_INTERPRETER));
                }
            }
            std::vector<Memory*> get(size_t n){
                reserve(n);
                std::vector<Memory*> ret;
                for(Memory* m : chunks){
                    if(ret.size() < n && m->usable){
                        ret.push_back(m);
                    }
                }
                return ret;
            }
        };
    }
}
#endif
