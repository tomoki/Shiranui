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
                return vars.find(id) != vars.end();

            }
            bool Environment::has(Identifier id) const{
                if(parent == nullptr){
                    return is_here(id);
                }else{
                    return is_here(id) or parent->has(id);
                }
            }
            sp<Value> Environment::get(Identifier id){
                if(is_here(id)){
                    return vars[id];
                }else{
                    if(parent == nullptr){
                        // throw exception?
                        return nullptr;
                    }else{
                        return parent->get(id);
                    }
                }
            }
            // ----- for rollback ---------------------------------
            void Environment::remove(Identifier id){
                if(vars.find(id) != vars.end()){
                    vars.erase(id);
                }else{
                    throw timemachine::TimeMachineException();
                }
            }
            void Environment::force_define(Identifier id,sp<Value> v){
                vars[id]= v;
            }
            // -----------------------------------------------
            void Environment::define(Identifier id,sp<Value> v){
                vars[id] = v;
                current_version++;
                changes.push_back(std::make_shared<timemachine::EnvDefineChange>(id,v));
            }
            void Environment::clear(){
                vars.clear();
                changes.clear();
                parent = nullptr;
            }
            std::ostream& operator<<(std::ostream& os,const Environment& e){
                using namespace runtime::value;
                if(e.parent != nullptr){
                    os << *(e.parent) << std::endl;
                }
                os << "===== layer  =====" << std::endl;
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
                for(auto p : e.vars){
                    if(filter.find(p.first) != filter.end()){
                        ret[p.first] = p.second;
                    }
                }
                return ret;
            }
        }
        namespace timemachine{
            EnvDefineChange::EnvDefineChange(syntax::ast::Identifier i,sp<value::Value> v)
                : id(i),value(v) {}
            void EnvDefineChange::rollback(sp<environment::Environment> target){
                target->remove(id);
            }
            void EnvDefineChange::flash(sp<environment::Environment> target){
                // do not record this as change.
                target->force_define(id,value);
            }
        }
    }

}
