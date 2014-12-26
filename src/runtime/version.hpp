#ifndef VERSION_HPP_INCLUDED
#define VERSION_HPP_INCLUDED

#include <map>

namespace shiranui{
    namespace runtime{
        namespace environment{
            struct Environment;
        }
        namespace value{
            struct Value;
        }
        namespace timemachine{
            using version = int;
            // using raw pointer because visitor doesn't has sp<Value> for argument
            using VersionMapForValue = std::map<value::Value*,version>;
            using VersionMapForEnvironment = std::map<environment::Environment*,version>;
            using VersionMap = std::pair<VersionMapForValue,VersionMapForEnvironment>;
            // template<typename T>
            // T move(T,const VersionMap&);

            // template<typename T>
            // VersionMap save(T);
        }
    }
}


#endif
