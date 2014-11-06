#include "dsl_runtime.hpp"
#include "dsl_exception.hpp"
#include <memory>

namespace shiranui{
    namespace runtime{
        namespace DSL{
            using namespace syntax::ast::DSL;
            struct DSLRunner : VisitorForDSL{
                sp<DSLInner> top;
                sp<value::Value> cur_v;
                // replace to ...
                std::map<sp<value::Value>,DSLVariable> var_occurrences;
                std::map<DSLVariable,sp<value::Value> > var_defines;
                std::map<syntax::ast::Identifier,sp<syntax::ast::Function> > marker_to_lambda;
                sp<environment::Environment> env;
                DSLRunner(sp<DSLInner> t,const decltype(marker_to_lambda) &m2l,
                          sp<environment::Environment> e)
                    : top(t),marker_to_lambda(m2l),env(e) {}
                void operator()(DSLVariable& node){
                    sp<value::Integer> place_holder = std::make_shared<value::Integer>(-1);
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
                void operator()(DSLInteger& node){
                    cur_v = std::make_shared<value::Integer>(node.value);
                }
                void operator()(DSLString& node){
                    cur_v = std::make_shared<value::String>(node.value);
                }
                void operator()(DSLArray& node){
                    std::vector<sp<value::Value> > ret;
                    for(auto p : node.value){
                        p->accept(*this);
                        ret.push_back(cur_v);
                    }
                    cur_v = std::make_shared<value::Array>(ret);
                }
                void operator()(DSLFunction& node){
                    sp<syntax::ast::Function> fast = marker_to_lambda[node.lambda_id];
                    if(fast == nullptr){
                        throw DSLUnknownVariable(top);
                    }
                    // parent is where dsl was evaled.
                    // TODO: is it correct?
                    sp<environment::Environment> fenv = std::make_shared<environment::Environment>(env);
                    for(auto p : node.environment){
                        syntax::ast::Identifier i(p.first->name);
                        auto pv = run_dsl(p.second,marker_to_lambda,env);
                        // define as mutable
                        fenv->define(i,pv,false);
                    }
                    cur_v = std::make_shared<value::UserFunction>(fast->parameters,fast->body,fenv);
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
                        if(var_occurrences.find(p) != var_occurrences.end()){
                            DSLVariable v = var_occurrences[p];
                            if(var_defines.find(v) == var_defines.end()){
                                throw DSLUnknownVariable(top);
                            }
                            p = var_defines[v];
                        }
                    }
                }
                void visit(value::UserFunction&){}
                void visit(value::Return&){}
                void visit(value::SystemCall&){}
                void visit(value::BuiltinFunction&){}
            };
            sp<value::Value> run_dsl(sp<DSLInner> dsl,
                                     const std::map<syntax::ast::Identifier,sp<syntax::ast::Function> >&marker_to_lambda,
                                     sp<environment::Environment> env){
                DSLRunner runner(dsl,marker_to_lambda,env);
                dsl->accept(runner);
                sp<value::Value> ret = runner.cur_v;
                DSLVariableReplacer replacer(dsl,runner.var_occurrences,runner.var_defines);
                ret->accept(replacer);
                return ret;
            }
        }
    }
}
