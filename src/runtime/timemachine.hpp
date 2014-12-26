#ifndef TIMEMACHINE_HPP_INCLUDED
#define TIMEMACHINE_HPP_INCLUDED

#include "version.hpp"
#include "../misc.hpp"


// timemachine
namespace shiranui{
    namespace runtime{
        namespace timemachine{
            class TimeMachineException{
            public:
                TimeMachineException() {};
                const char* what() const noexcept {return "TimeMachineException";}
            };
            sp<value::Value> move(sp<value::Value>,const VersionMap&);
            VersionMap save(sp<value::Value>);
        }
    }
}

#endif
