#ifndef TIMEMACHINE_HPP_INCLUDED
#define TIMEMACHINE_HPP_INCLUDED

#include "version.hpp"
#include "../misc.hpp"
#include "change.hpp"


// timemachine
namespace shiranui{
    namespace runtime{
        namespace timemachine{
            sp<value::Value> move(sp<value::Value>,const VersionMap&);
            VersionMap save(sp<value::Value>);
        }
    }
}

#endif
