#include "dsl_runtime.hpp"
#include "dsl_exception.hpp"
#include <memory>

namespace shiranui{
    namespace runtime{
        namespace DSL{
            using namespace syntax::ast::DSL;
            struct DSLRunner : VisitorForDSL{
                Memory* memory;
                sp<DSLInner> top;
                sp<value::Value> cur_v;
                // replace to ...
                std::map<sp<value::Value>,DSLVariable> var_occurrences;
                std::map<DSLVariable,sp<value::Value> > var_defines;
                std::map<syntax::ast::Identifier,sp<syntax::ast::Function> > marker_to_lambda;
                sp<environment::Environment> env;
                DSLRunner(Memory* memory_, sp<DSLInner> t,const decltype(marker_to_lambda) &m2l,
                          sp<environment::Environment> e)
                    : memory(memory_), top(t),marker_to_lambda(m2l),env(e) {}
                void operator()(DSLVariable& node){
                    sp<value::Integer> place_holder = memory->create<value::Integer>(-1);
                    var_occurrences[place_holder] = node;
                    cur_v = place_holder;
                }
                void operator()(DSLDefine& node){
                    node.value->accept(*this);
                    if(var_defines[*node.var] != nullptr){
                        throw DSLAlreadyUsedVariable(top);
                    }
                    var_defines[*node.var] = cur_v;
                }
                void operator()(DSLBoolean& node){
                    cur_v = memory->create<value::Boolean>(node.value);
                }
                void operator()(DSLInteger& node){
                    cur_v = memory->create<value::Integer>(node.value);
                }
                void operator()(DSLString& node){
                    cur_v = memory->create<value::String>(node.value);
                }
                void operator()(DSLArray& node){
                    std::vector<sp<value::Value> > ret;
                    for(auto p : node.value){
                        p->accept(*this);
                        ret.push_back(cur_v);
                    }
                    cur_v = memory->create<value::Array>(ret);
                }
                void operator()(DSLFunction& node){
                    sp<syntax::ast::Function> fast = marker_to_lambda[node.lambda_id];
                    if(fast == nullptr){
                        throw DSLUnknownVariable(top);
                    }
                    // parent is where dsl was evaled.
                    // TODO: is it correct?
                    sp<environment::Environment> fenv = memory->create<environment::Environment>(memory, env);
                    for(auto p : node.environment){
                        syntax::ast::Identifier i(p.first->name);
                        p.second->accept(*this);
                        fenv->define(i,cur_v);
                    }
                    cur_v = memory->create<value::UserFunction>(fast->parameters,fast->body,fenv);
                }
                void operator()(DSLRef& node){
                    node.to->accept(*this);
                    cur_v = memory->create<value::Ref>(cur_v);
                }
            };
            // replace all variable occurences in DSL
            struct DSLVariableReplacer : value::VisitorForValue{
                sp<DSLInner> top;
                std::map<sp<value::Value>,DSLVariable> var_occurrences;
                std::map<DSLVariable,sp<value::Value> > var_defines;
                DSLVariableReplacer(sp<DSLInner> t,
                                    std::map<sp<value::Value>,DSLVariable> occurs,
                                    std::map<DSLVariable,sp<value::Value> > defines)
                    : top(t),var_occurrences(occurs),var_defines(defines){}
                void visit(value::Integer&){}
                void visit(value::String&){}
                void visit(value::Boolean&){}
                void visit(value::Array& node){
                    for(auto& p : node.value){
                        p->accept(*this);
                        if(is_var(p)){
                            p = find(p);
                        }
                    }
                }
                void visit(value::UserFunction& node){
                    // env defined by DSL always in top of env
                    for(auto& p : node.env->vars){
                        p.second->accept(*this);
                        if(is_var(p.second)){
                            node.env->define(p.first,find(p.second));
                        }
                    }
                }
                void visit(value::Ref& node){
                    // TODO:writehere
                    node.to->accept(*this);
                    if(is_var(node.to)){
                        node.to = find(node.to);
                    }
                }
                void visit(value::Return&){}
                void visit(value::SystemCall&){}
                void visit(value::BuiltinFunction&){}
                bool is_var(sp<value::Value> p){
                    return var_occurrences.find(p) != var_occurrences.end();
                }
                sp<value::Value> find(sp<value::Value> p){
                    DSLVariable v = var_occurrences[p];
                    if(var_defines.find(v) == var_defines.end()){
                        throw DSLUnknownVariable(top);
                    }
                    return var_defines[v];
                }
            };
            sp<value::Value> run_dsl(Memory* memory,
                                     sp<DSLInner> dsl,
                                     const std::map<syntax::ast::Identifier,sp<syntax::ast::Function> >&marker_to_lambda,
                                     sp<environment::Environment> env){
                DSLRunner runner(memory,dsl,marker_to_lambda,env);
                dsl->accept(runner);
                sp<value::Value> ret = runner.cur_v;
                DSLVariableReplacer replacer(dsl,runner.var_occurrences,runner.var_defines);
                ret->accept(replacer);
                return ret;
            }
        }
    }
}
