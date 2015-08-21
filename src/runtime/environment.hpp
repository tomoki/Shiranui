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
                timemachine::version current_version = 0;
                Memory* memory;
                sp<Environment> parent;
                std::vector<sp<timemachine::ChangeEnv> > changes;
                Environment(Memory*);
                Environment(Memory*, sp<Environment> parent);
                std::map<syntax::ast::Identifier,sp<value::Value>> vars;
                bool is_here(syntax::ast::Identifier id) const;
                bool has(syntax::ast::Identifier id) const;
                sp<value::Value> get(syntax::ast::Identifier);
                void remove(syntax::ast::Identifier);
                void force_define(syntax::ast::Identifier,sp<value::Value>);
                void define(syntax::ast::Identifier,sp<value::Value>);
                void clear();
            };
            std::ostream& operator<<(std::ostream&,const Environment&);
            std::map<syntax::ast::Identifier,
                     sp<value::Value> >
            filter_environment(Environment&, std::set<syntax::ast::Identifier>);
        }
        namespace timemachine{
            struct EnvDefineChange : ChangeEnv{
                syntax::ast::Identifier id;
                sp<value::Value> value;
                EnvDefineChange(syntax::ast::Identifier,sp<value::Value>);
                void rollback(sp<environment::Environment>);
                void flash(sp<environment::Environment>);
            };
        }
    }
}
#endif
