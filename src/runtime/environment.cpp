#include "environment.hpp"

using shiranui::runtime::value::Value;
using shiranui::syntax::ast::Identifier;
namespace shiranui{
    namespace runtime{
        namespace environment{
            Environment::Environment(){
            }
            Environment::Environment(Environment* par)
                : parent(par){
            }
            Environment::Environment(sp<Environment> par)
                : parent(par){
            }

            bool Environment::is_here(Identifier id) const{
                return vars.find(id) != vars.end()
                    or consts.find(id) != consts.end();

            }
            bool Environment::has(Identifier id) const{
                if(parent == nullptr){
                    return is_here(id);
                }else{
                    return is_here(id) or parent->has(id);
                }
            }
            bool Environment::is_const(Identifier id) const{
                if(is_here(id)){
                    return consts.find(id) != consts.end();
                }else if(parent == nullptr){
                    return false;
                }else{
                    return parent->is_const(id);
                }
            }
            sp<Value> Environment::get(Identifier id){
                if(is_here(id)){
                    if(is_const(id)){
                        return consts[id];
                    }else{
                        return vars[id];
                    }
                }else{
                    if(parent == nullptr){
                        // throw exception?
                        return nullptr;
                    }else{
                        return parent->get(id);
                    }
                }
            }
            void Environment::set(Identifier id,sp<Value> v){
                if(is_here(id)){
                    vars[id] = v;
                }else{
                    parent->set(id,v);
                }
            }
            void Environment::define(Identifier id,sp<Value> v,bool is_const){
                if(is_const){
                    consts[id] = v;
                }else{
                    vars[id] = v;
                }
            }
        }
    }
}
