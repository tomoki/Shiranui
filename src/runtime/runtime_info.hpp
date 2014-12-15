#ifndef RUNTIME_INFO_HPP_INCLUDED
#define RUNTIME_INFO_HPP_INCLUDED

#include <vector>
#include <unordered_map>
#include <algorithm>
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
                    call_under.clear();
                    memo.clear();
                }
                int index_of_called(int under){
                    if(call_under.find(under) == call_under.end()) return -1;
                    int id = call_under[under];
                    int index = lower_bound(visit_time.begin(),
                                            visit_time.end(),
                                            id) - visit_time.begin();
                    return index;
                }
            };
        }
    }
}
#endif
