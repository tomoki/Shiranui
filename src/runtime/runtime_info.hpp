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
            struct RuntimeInfomation{
                std::vector<int> visit_time;
                std::unordered_map<int,sp<value::Value>> return_value;
                std::unordered_map<int,std::string> memo;
            };
        }
    }
}
#endif
