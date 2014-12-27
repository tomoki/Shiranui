#ifndef RUNTIME_INFO_HPP_INCLUDED
#define RUNTIME_INFO_HPP_INCLUDED

#include <vector>
#include <unordered_map>
#include <algorithm>
#include "misc.hpp"
#include "version.hpp"
#include "timemachine.hpp"

namespace shiranui{
    namespace syntax{
        namespace ast{
            struct Block;
        }
    }
}

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
            using ReturnValue = std::pair<sp<value::Value>,timemachine::VersionMap>;

            template<typename T>
            std::pair<int,sp<runtime::value::Value>> return_value(T& ast_node,
                                                                  int call_under_id);
            template<typename T>
            std::pair<int,sp<runtime::value::Value>> return_value(T& ast_node,
                                                                  int call_under_id){
                if(ast_node.runtime_info.call_under.find(call_under_id)
                   != ast_node.runtime_info.call_under.end()){
                    int id = ast_node.runtime_info.call_under[call_under_id];
                    auto p = ast_node.runtime_info.return_value[id];
                    if(p.first == nullptr){
                        return std::make_pair(-2,nullptr);
                    }
                    return std::make_pair(id,timemachine::move(p.first,p.second));
                }else{
                    return std::make_pair(-2,nullptr);
                }
            }
            ReturnValue make_return_value(sp<value::Value> v);
            // ReturnValue make_return_value(sp<value::Value> v){
            //     using namespace timemachine;
            //     return ReturnValue(v,save(v));
            // }
            struct RuntimeInfomation{
                std::vector<int> visit_time;
                // TODO:for can't handle well.
                std::map<int,int> call_under;
                std::map<int,std::pair<int,sp<syntax::ast::Block> > > up;
                // std::unordered_map<int,sp<value::Value>> return_value;
                std::map<int,ReturnValue> return_value;
                std::map<int,std::string> memo;

                void clear(){
                    return_value.clear();
                    call_under.clear();
                    memo.clear();
                    up.clear();
                }
                int index_of_called(int under){
                    if(call_under.find(under) == call_under.end()) return -1;
                    int id = call_under[under];
                    int index = lower_bound(visit_time.begin(),
                                            visit_time.end(),
                                            id) - visit_time.begin();
                    return index;
                }
                std::pair<int,sp<syntax::ast::Block> > get_up(int i){
                    if(up.find(i) != up.end()){
                        return up[i];
                    }else{
                        return std::make_pair(-123,nullptr);
                    }
                }
            };

        }
    }
}
#endif
