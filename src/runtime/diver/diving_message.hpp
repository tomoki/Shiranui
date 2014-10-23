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
