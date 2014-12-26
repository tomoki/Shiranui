#ifndef CHANGE_HPP_INCLUDED
#define CHANGE_HPP_INCLUDED

#include "../misc.hpp"
#include "version.hpp"

namespace shiranui{
    namespace runtime{
        namespace timemachine{
            struct Change{
            };
            struct ChangeValue : Change{
                virtual void rollback(sp<value::Value>) = 0;
                virtual void flash(sp<value::Value>)    = 0;
            };
            struct ChangeEnv : Change{
                virtual void rollback(sp<environment::Environment>) = 0;
                virtual void flash(sp<environment::Environment>)    = 0;
            };
        }
    }
}

#endif
