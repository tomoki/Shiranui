#ifndef DSL_RUNTIME_HPP
#define DSL_RUNTIME_HPP

#include "../value.hpp"
#include "../../syntax/ast.hpp"
#include "../environment.hpp"
#include <map>
#include <vector>
#include <exception>

namespace shiranui{
    namespace runtime{
        namespace DSL{
            sp<value::Value> run_dsl(Memory*,
                                     sp<syntax::ast::DSL::DSLInner>,
                                     const std::map<syntax::ast::Identifier,sp<syntax::ast::Function> >&,
                                     sp<environment::Environment>);
        }
    }
}
#endif
