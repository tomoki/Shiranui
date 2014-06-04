#ifndef ENVIRONMENT_HPP_INCLUDED
#define ENVIRONMENT_HPP_INCLUDED

#include "value.hpp"
#include "../syntax/ast.hpp"
#include <map>

namespace shiranui{
    namespace environment{
        using shiranui::syntax::ast::Identifier;
        using shiranui::syntax::ast::Variable;
        using shiranui::runtime::value::Value;
        struct Environment{
            Environment() {};
            // should identify const or not
            std::map<Identifier,sp<Value>> map;
            bool has(Identifier id){
                return map.find(id) != map.end();
            }
            bool has(Variable var){
                return map.find(var.value) != map.end();
            }

            sp<Value> get(Identifier id){
                return map[id];
            }
            sp<Value> get(Variable var){
                return map[var.value];
            }
            void set(Identifier id,Value* v){
                map[id] = sp<Value>(v);
            }
            void set(Identifier id,sp<Value> v){
                map[id] = sp<Value>(v);
            }
        };
    }
}
#endif
