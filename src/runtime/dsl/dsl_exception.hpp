#ifndef DSL_EXCEPTION
#define DSL_EXCEPTION

#include "../value.hpp"
#include "../../syntax/ast.hpp"
#include <exception>

namespace shiranui{
    namespace runtime{
        namespace DSL{
            struct DSLException : std::exception{
            };
            struct DSLUnknownVariable : std::exception{
                sp<syntax::ast::DSL::DSLInner> where;
                DSLUnknownVariable(sp<syntax::ast::DSL::DSLInner> w) : where(w) {};
            };
            struct DSLAlreadyUsedVariable : std::exception{
                sp<syntax::ast::DSL::DSLInner> where;
                DSLAlreadyUsedVariable(sp<syntax::ast::DSL::DSLInner> w) : where(w) {};
            };
        }
    }
}
#endif
