#ifndef ENVIRONMENT_HPP_INCLUDED
#define ENVIRONMENT_HPP_INCLUDED

#include "value.hpp"
#include "../syntax/ast.hpp"
#include <map>

namespace shiranui{
    namespace runtime{
        namespace environment{
            using shiranui::syntax::ast::Identifier;
            using shiranui::syntax::ast::Variable;
            using shiranui::runtime::value::Value;
            struct Environment{
                Environment();
                Environment(sp<Environment> parent);
                Environment(Environment *parent);
                sp<Environment> parent;
                std::map<Identifier,sp<Value>> vars;
                std::map<Identifier,sp<Value>> consts;
                bool is_here(Identifier id) const;
                bool has(Identifier id) const;
                bool is_const(Identifier) const;
                sp<Value> get(Identifier);
                void set(Identifier,sp<Value>);
                void define(Identifier,sp<Value>,bool);
            };
        }
    }
}
#endif
