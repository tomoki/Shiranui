#include "timemachine.hpp"
#include "value.hpp"
#include "environment.hpp"
#include <set>

namespace shiranui{
    namespace runtime{
        namespace timemachine{
            using namespace value;
            using namespace environment;

            using VisitedValue  = std::set<Value*>;
            using VisitedEnv = std::set<Environment*>;

            template<typename P>
            void store_value_version(VersionMap& vm,P v){
                auto p = *&v; // handle shared_ptr too.
                vm.first[p] = p->current_version;
            }

            template<typename T>
            void save(T,VersionMap&,VisitedValue&,VisitedEnv&);

            template<typename P>
            void save_env(P v,VersionMap& vm,VisitedValue& vv,VisitedEnv& ve){
                auto p = &*v;
                if(ve.find(p) != ve.end()) return;
                ve.insert(p);
                vm.second[p] = v->current_version;
                for(const auto& m : {v->vars,v->consts}){
                    for(auto p : m){
                        save(p.second,vm,vv,ve);
                    }
                }
                if(v->parent != nullptr){
                    save_env(v->parent,vm,vv,ve);
                }
            }

            version get_stored_version(const VersionMap& vm,Value* p){
                const auto m = vm.first;
                if(m.find(p) == m.end()){
                    throw TimeMachineException();
                }
                return m.at(p);
            }
            version get_stored_version(const VersionMap& vm,sp<Value> p){
                return get_stored_version(vm,&*p);
            }

            version get_stored_version(const VersionMap& vm,Environment* p){
                const auto m = vm.second;
                if(m.find(p) == m.end()){
                    throw TimeMachineException();
                }
                return m.at(p);
            }
            version get_stored_version(const VersionMap& vm,sp<Environment> p){
                return get_stored_version(vm,&*p);
            }

            void move(sp<Environment> v, const VersionMap& vm,
                      VisitedValue& vv,VisitedEnv& ve);


            void send_time(Value* v,version move_to){
                if(v->current_version < move_to){
                    for(int i=v->current_version;i<move_to;i++){
                        v->changes[i]->flash(v);
                    }
                }else{
                    for(int i=v->current_version-1;i>=move_to;i--){
                        v->changes[i]->rollback(v);
                    }
                }
                v->current_version = move_to;
            }
            struct TimeScale : VisitorForValue{
                VersionMap vm;
                VisitedValue& vv;
                VisitedEnv& ve;
                TimeScale(const VersionMap& vm_,
                          VisitedValue& vv_,
                          VisitedEnv& ve_)
                    : vm(vm_),vv(vv_),ve(ve_){}
                // there is no need to do something
                //  because they don't have version.
                void visit(Integer&){}
                void visit(String&){}
                void visit(Boolean&){}
                void visit(Return&){}
                void visit(SystemCall&){}
                void visit(BuiltinFunction&){}

                void visit(Array& node){
                    if(vv.find(&node) != vv.end()){return;}
                    vv.insert(&node);
                    // TODO: It is naive implementation
                    //  I should store which index store array or function
                    version move_to = get_stored_version(vm,&node);
                    send_time(&node,move_to);
                    // restore value.
                    for(sp<Value> v : node.value){
                        v->accept(*this);
                    }
                }
                void visit(UserFunction& node){
                    if(vv.find(&node) != vv.end()){return;}
                    move(node.env,vm,vv,ve);
                }
            };
            // TODO:do not recursive.
            struct VersionDisk : VisitorForValue{
                VersionMap& vm;
                VisitedValue& vv;
                VisitedEnv& ve;
                VersionDisk(VersionMap& vm_,
                            VisitedValue& vv_,
                            VisitedEnv& ve_)
                    : vm(vm_),vv(vv_),ve(ve_){}

                void visit(Integer&){}
                void visit(String&){}
                void visit(Boolean&){}
                void visit(Return&){}
                void visit(SystemCall&){}
                void visit(BuiltinFunction&){}

                // TODO: It is naive implementation
                //  I should store which index store array or function
                void visit(Array& node){
                    if(vv.find(&node) != vv.end()){return;}
                    vv.insert(&node);
                    store_value_version(vm,&node);
                    for(sp<Value> v : node.value){
                        v->accept(*this);
                    }
                }
                void visit(UserFunction& node){
                    if(vv.find(&node) != vv.end()){return;}
                    vv.insert(&node);
                    store_value_version(vm,&node);
                    save_env(node.env,vm,vv,ve);
                }
            };

            template<typename T>
            T move(T v,const VersionMap& vm,VisitedValue& vv,
                                            VisitedEnv& ve){
                TimeScale ts(vm,vv,ve);
                v->accept(ts);
                return v;
            }
            sp<Value> move(sp<Value> v,const VersionMap& vm){
                VisitedValue vv;
                VisitedEnv ve;
                return move(v,vm,vv,ve);
            }

            // helper
            void move(sp<Environment> v, const VersionMap& vm,
                      VisitedValue& vv,VisitedEnv& ve){
                if(ve.find(&*v) != ve.end()) return;
                ve.insert(&*v);
                // TODO:rollback environment
                version move_to = get_stored_version(vm,v);
                // restore

                for(const auto& m : {v->vars,v->consts}){
                    for(auto p : m){
                        move(p.second,vm,vv,ve);
                    }
                }
                if(v->parent != nullptr){
                    move(v->parent,vm,vv,ve);
                }
            }

            template<typename T>
            void save(T v,VersionMap& vm,
                      VisitedValue& vv,VisitedEnv& ve){
                VersionDisk vd(vm,vv,ve);
                v->accept(vd);
            }
            VersionMap save(sp<Value> v){
                VersionMap vm;
                VisitedValue vv;
                VisitedEnv ve;
                save(v,vm,vv,ve);
                return vm;
            }
        }
    }
}
