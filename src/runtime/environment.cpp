#include "environment.hpp"
#include "value_printer.hpp"

using shiranui::runtime::value::Value;
using shiranui::syntax::ast::Identifier;
namespace shiranui{
    namespace runtime{
        namespace environment{
            Environment::Environment(){
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
                    current_version++;
                    changes.push_back(std::make_shared<timemachine::EnvSetChange>(id,vars[id],v));
                    vars[id] = v;
                }else{
                    parent->set(id,v);
                }
            }
            // ----- for rollback ---------------------------------
            void Environment::remove(Identifier id){
                if(vars.find(id) != vars.end()){
                    vars.erase(id);
                }else if(consts.find(id) != consts.end()){
                    consts.erase(id);
                }else{
                    throw timemachine::TimeMachineException();
                }
            }
            void Environment::force_set(Identifier id,sp<Value> v){
                if(vars.find(id) != vars.end()){
                    vars.erase(id);
                }else if(consts.find(id) != consts.end()){
                    consts.erase(id);
                }else{
                    throw timemachine::TimeMachineException();
                }
            }
                // -----------------------------------------------

            void Environment::define(Identifier id,sp<Value> v,bool is_const){
                if(is_const){
                    consts[id] = v;
                }else{
                    vars[id] = v;
                }
                current_version++;
                changes.push_back(std::make_shared<timemachine::EnvDefineChange>(id,v,is_const));
            }
            void Environment::clear(){
                vars.clear();
                consts.clear();
                changes.clear();
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
        namespace timemachine{
            EnvSetChange::EnvSetChange(syntax::ast::Identifier i,sp<Value> p,sp<Value> n)
                : id(i),prev(p),next(n) {}
            void EnvSetChange::rollback(sp<environment::Environment> target){
                // prev will not be nullptr
                target->force_set(id,prev);
            }
            void EnvSetChange::flash(sp<environment::Environment> target){
                target->force_set(id,next);
            }
            EnvDefineChange::EnvDefineChange(syntax::ast::Identifier i,sp<value::Value> v,bool is_const_)
                : id(i),value(v),is_const(is_const_) {}
            void EnvDefineChange::rollback(sp<environment::Environment> target){
                target->remove(id);
            }
            void EnvDefineChange::flash(sp<environment::Environment> target){
                target->define(id,value,is_const);
            }
        }
    }

}
