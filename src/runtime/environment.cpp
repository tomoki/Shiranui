#include "environment.hpp"
#include "value_printer.hpp"

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
            void Environment::clear(){
                vars.clear();
                consts.clear();
                parent = nullptr;
            }
            std::ostream& operator<<(std::ostream& os,const Environment& e){
                using namespace runtime::value;
                if(e.parent != nullptr){
                    os << *(e.parent) << std::endl;
                }
                os << "===== layer  =====" << std::endl;
                os << "---- consts ----" << std::endl;
                for(const auto p : e.consts){
                    os << p.first.name << " -> " << to_reproductive(p.second) << std::endl;
                }
                os << "---- var    ----" << std::endl;
                for(const auto p : e.vars){
                    os << p.first.name << " -> " << to_reproductive(p.second) << std::endl;
                }
                os << "==================" << std::endl;
                return os;
            }

            std::map<syntax::ast::Identifier,
                     sp<value::Value> >
            filter_environment(const Environment& e,std::set<syntax::ast::Identifier> filter){
                std::map<syntax::ast::Identifier,sp<value::Value> > ret;
                // first,filter parent
                if(e.parent != nullptr){
                    ret = filter_environment(*(e.parent),filter);
                }

                // overwrite parent environment
                for(auto m : {e.vars,e.consts}){
                    for(auto p : m){
                        if(filter.find(p.first) != filter.end()){
                            ret[p.first] = p.second;
                        }
                    }
                }
                return ret;
            }
        }
    }
}
