#ifndef DIVING_MESSAGE_HPP_INCLUDED
#define DIVING_MESSAGE_HPP_INCLUDED

#include <string>
#include <sstream>
#include "../../syntax/ast.hpp"

namespace shiranui{
    namespace runtime{
        namespace diver{
            const std::string STRIKE = "strike";
            const std::string EXPLORE = "explore";
            const std::string ERROR = "error";
            const std::string FLYMARK_RESULT = "flymark_result";
            struct DivingMessage{
                std::string cache;
                // std::string str();
                std::string str(){
                    return cache;
                }

                template<typename T>
                DivingMessage add_strike(const T& t){
                    std::stringstream ss;
                    ss << cache << STRIKE << std::endl
                       << t.point << " " << t.length << std::endl;
                    cache = ss.str();
                    return *this;
                }
                template<typename T>
                DivingMessage add_explore(const T& t,const std::string& what){
                    std::stringstream ss;
                    ss << cache << EXPLORE << std::endl
                       << t.point << " " << t.length << std::endl
                       << what << std::endl;
                    cache = ss.str();
                    return *this;
                }
                template<typename T>
                DivingMessage add_error(const T& t,const std::string& what){
                    std::stringstream ss;
                    ss << cache << ERROR << std::endl
                       << t.point << " " << t.length << std::endl
                       << what << std::endl;
                    cache = ss.str();
                    return *this;
                }
                DivingMessage add_flymark_result(syntax::ast::FlyMark& t,const std::string& what){
                    std::stringstream ss;
                    int start_point = t.point;
                    int end_point = t.point + t.length;
                    int insert_point = end_point - 1;
                    int remove_length = 0;
                    if(t.right.size() != 0){
                        insert_point = t.right[0]->point;
                        remove_length = (t.right.back()->point + t.right.back()->length)
                                        - t.right[0]->point;
                    }
                    ss << cache << FLYMARK_RESULT << std::endl
                       << start_point << " " << t.length << std::endl
                       << insert_point << " " << remove_length << std::endl
                       << what << std::endl;
                    cache = ss.str();
                    return *this;
                }
                DivingMessage operator+(DivingMessage message){
                    DivingMessage ret;
                    ret.cache = cache + message.cache;
                    return ret;
                }
            };
        }
    }
}
#endif
