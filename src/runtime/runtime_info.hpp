#ifndef RUNTIME_INFO_HPP_INCLUDED
#define RUNTIME_INFO_HPP_INCLUDED

#include <vector>
#include <unordered_map>
#include "value.hpp"

namespace shiranui{
    namespace runtime{
        namespace value{
            struct Value;
        }
    }
}

namespace shiranui{
    namespace runtime{
        namespace infomation{
            const int COUNT_FROM = 0;
            const int TOPLEVEL = -1;
            struct RuntimeInfomation{
                std::vector<int> visit_time;
                // TODO:for can't handle well.
                std::unordered_map<int,int> call_under;
                std::unordered_map<int,sp<value::Value>> return_value;
                std::unordered_map<int,std::string> memo;

                void clear(){
                    return_value.clear();
                }
            };
        }
    }
}
#endif
