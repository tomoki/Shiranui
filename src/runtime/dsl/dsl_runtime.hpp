#ifndef DSL_RUNTIME_HPP
#define DSL_RUNTIME_HPP

#include "../value.hpp"
#include "../../syntax/ast.hpp"

#include <map>
#include <vector>
#include <exception>

namespace shiranui{
    namespace runtime{
        namespace DSL{
            sp<value::Value> run_dsl(sp<syntax::ast::DSL::DSLInner>);
        }
    }
}
#endif
