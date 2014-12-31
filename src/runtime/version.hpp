#ifndef VERSION_HPP_INCLUDED
#define VERSION_HPP_INCLUDED

#include <map>
#include <string>

namespace shiranui{
    namespace runtime{
        namespace environment{
            struct Environment;
        }
        namespace value{
            struct Value;
        }
        namespace timemachine{
            class TimeMachineException{
                std::string message;
            public:
                TimeMachineException() : message("") {};
                TimeMachineException(std::string s) : message(s) {};
                const char* what() const noexcept {return ("TimeMachineException: " + message).c_str();}
            };
            using version = int;
            // using raw pointer because visitor doesn't has sp<Value> for argument
            using VersionMapForValue = std::map<value::Value*,version>;
            using VersionMapForEnvironment = std::map<environment::Environment*,version>;
            using VersionMap = std::pair<VersionMapForValue,VersionMapForEnvironment>;
        }
    }
}


#endif
