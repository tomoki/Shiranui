#include "runtime_info.hpp"
#include "value.hpp"
#include "timemachine.hpp"

namespace shiranui{
    namespace runtime{
        namespace infomation{
            // template<typename T>
            // std::pair<int,sp<runtime::value::Value>> return_value(T& ast_node,
            //                                                       int call_under_id){
            //     if(ast_node.runtime_info.call_under.find(call_under_id)
            //        != ast_node.runtime_info.call_under.end()){
            //         int id = ast_node.runtime_info.call_under[call_under_id];
            //         auto p = ast_node.runtime_info.return_value[id];
            //         return std::make_pair(id,timemachine::move(p.first,p.second));
            //     }else{
            //         return std::make_pair(-2,nullptr);
            //     }
            // }
            ReturnValue make_return_value(sp<value::Value> v){
                using namespace timemachine;
                return ReturnValue(v,save(v));
            }
        }
    }
}

