#ifndef ENVIRONMENT_HPP_INCLUDED
#define ENVIRONMENT_HPP_INCLUDED

#include "value.hpp"
#include "version.hpp"
#include "../syntax/ast.hpp"
#include <map>
#include <set>

namespace shiranui{
    namespace runtime{
        namespace environment{
            struct Environment{
                timemachine::version current_version;
                Environment();
                Environment(sp<Environment> parent);
                Environment(Environment *parent);
                sp<Environment> parent;
                std::map<syntax::ast::Identifier,sp<value::Value>> vars;
                std::map<syntax::ast::Identifier,sp<value::Value>> consts;
                bool is_here(syntax::ast::Identifier id) const;
                bool has(syntax::ast::Identifier id) const;
                bool is_const(syntax::ast::Identifier) const;
                sp<value::Value> get(syntax::ast::Identifier);
                void set(syntax::ast::Identifier,sp<value::Value>);
                void remove(syntax::ast::Identifier);
                void force_set(syntax::ast::Identifier,sp<value::Value>);
                void define(syntax::ast::Identifier,sp<value::Value>,bool);
                void clear();
            };
            std::ostream& operator<<(std::ostream&,const Environment&);
            std::map<syntax::ast::Identifier,
                     sp<value::Value> >
            filter_environment(const Environment&,std::set<syntax::ast::Identifier>);
        }
        namespace timemachine{
            struct EnvSetChange : ChangeEnv{
                syntax::ast::Identifier id;
                sp<value::Value> prev,next; // prev is null if no binding previously.
                EnvSetChange(syntax::ast::Identifier,sp<value::Value>,sp<value::Value>);
                void rollback(sp<environment::Environment>);
                void flash(sp<environment::Environment>);
            };
            struct EnvDefineChange : ChangeEnv{
                syntax::ast::Identifier id;
                sp<value::Value> value;
                bool is_const;
                EnvDefineChange(syntax::ast::Identifier,sp<value::Value>,bool);
                void rollback(sp<environment::Environment>);
                void flash(sp<environment::Environment>);
            };
        }
    }
}
#endif
