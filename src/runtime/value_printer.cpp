#include "value_printer.hpp"
#include "environment.hpp"
#include "../syntax/lambda_man.hpp"
#include <sstream>

namespace shiranui{
    namespace runtime{
        namespace value{
            std::string next_name(std::string s){
                if(s == "z"){
                    return "aa";
                }
                if(s.back() == 'z'){
                    return next_name(s.substr(0,s.size()-1)) + "a";
                }
                s.back() = s.back()+1;
                return s;
            }

            // find value appearing twice
            struct ValueScanner : VisitorForValue{
                sp<ast::SourceCode> code;
                std::map<Value*,int> cnt;
                ValueScanner(sp<ast::SourceCode> code_)
                    : code(code_) {};

                void visit(Integer& node){}
                void visit(String& node){}
                void visit(Boolean& node){}
                void visit(Array& node){
                    cnt[&node]++;
                    if(cnt[&node] >= 2) return;
                    for(sp<Value> p : node.value){
                        p->accept(*this);
                    }
                }
                void visit(UserFunction& node){
                    cnt[&node]++;
                    if(cnt[&node] >= 2) return;
                    if(code != nullptr){
                        if(code->where_is_function_from.find(node.body)
                           != code->where_is_function_from.end()){
                            sp<ast::Function> f = code->where_is_function_from[node.body];
                            auto syntactic_frees = syntax::scan_free_variable(f);
                            auto free_vars = filter_environment(node.env,syntactic_frees);
                            for(auto p : free_vars){
                                p.second->accept(*this);
                            }
                        }
                    }
                }
                void visit(Return& node){cnt[&node]++;}
                void visit(SystemCall& node){cnt[&node]++;}
                void visit(BuiltinFunction& node){cnt[&node]++;}
                void visit(Ref& node){
                    cnt[&node]++;
                    if(cnt[&node] >= 2) return;
                    node.to->accept(*this);
                }
            };


            PrettyPrinterForValue::PrettyPrinterForValue(std::ostream& os_,
                                                         std::map<Value*,int> c,
                                                         sp<ast::SourceCode> cod)
                : os(os_),cur_name("a"),cnt(c),found_recursive(false),code(cod){
                if(code != nullptr){
                    where_is_function_from = code->where_is_function_from;
                }
            }
            bool PrettyPrinterForValue::already_appeared(Value* p){
                return name.find(p) != name.end();
            }
            void PrettyPrinterForValue::use_prev_name(Value* p){
                os << name[p];
            }
            void PrettyPrinterForValue::register_name(Value* p){
                // if doesn't appear twice,there is no need to name.
                if(cnt[p] >= 2){
                    os << cur_name << "=";
                    name[p] = cur_name;
                    cur_name = next_name(cur_name);
                }
            }
            bool PrettyPrinterForValue::check_already_occured(Value* p){
                if(already_appeared(p)){
                    use_prev_name(p);
                    found_recursive = true;
                    return true;
                }
                register_name(p);
                return false;
            }
            void PrettyPrinterForValue::visit(Integer& node){
                if(check_already_occured(&node)) return;
                os << node.value;
            }
            void PrettyPrinterForValue::visit(String& node){
                if(check_already_occured(&node)) return;
                os << '"' << node.value << '"';
            }
            void PrettyPrinterForValue::visit(Boolean& node){
                if(check_already_occured(&node)) return;
                os << (node.value?"true":"false");
            }
            void PrettyPrinterForValue::visit(Array& node){
                if(check_already_occured(&node)) return;
                os << "[";
                for(int i=0;i<static_cast<int>(node.value.size());i++){
                    node.value[i]->accept(*this);
                    if(i != static_cast<int>(node.value.size())-1){
                        os << ",";
                    }
                }
                os << "]";
            }
            void PrettyPrinterForValue::visit(UserFunction& node){
                if(check_already_occured(&node)) return;
                // please make me DSL!
                found_recursive = true;
                if(where_is_function_from.find(node.body) != where_is_function_from.end()){
                    sp<ast::Function> f = where_is_function_from[node.body];
                    if(f->lambda_id.name.size() == 0){
                        os << "$()no_name";
                    }else{
                        auto syntactic_frees = syntax::scan_free_variable(f);

                        // if global has var,they should not be included
                        std::map<syntax::ast::Identifier,sp<value::Value> > free_not_global_vars;
                        {
                            auto free_vars = filter_environment(node.env,syntactic_frees);
                            auto global_env = node.env;
                            while(global_env->parent != nullptr){
                                global_env = global_env->parent;
                            }
                            for(auto p : free_vars){
                                if(global_env->has(p.first) and !is_ref_or_array(p.second)) continue;
                                free_not_global_vars[p.first] = p.second;
                            }
                        }

                        os << "$";
                        os << "(";
                        for(auto it = free_not_global_vars.begin();
                            it != free_not_global_vars.end();++it){
                            os << it->first.name << "->";
                            it->second->accept(*this);
                            if(std::next(it) != free_not_global_vars.end()){
                                os << ",";
                            }
                        }
                        os << ")";
                        os << f->lambda_id.name;
                    }
                }else{
                    os << "$()unknown";
                }
            }
            void PrettyPrinterForValue::visit(Return& node){
                if(check_already_occured(&node)) return;
                os << "(return ";
                node.value->accept(*this);
                os << ")";
            }
            // builtin
            void PrettyPrinterForValue::visit(SystemCall& node){
                if(check_already_occured(&node)) return;
                os << "system_call";
            }

            void PrettyPrinterForValue::visit(BuiltinFunction& node){
                if(check_already_occured(&node)) return;
                os << node.name;
            }
            void PrettyPrinterForValue::visit(Ref& node){
                if(check_already_occured(&node)) return;
                os << "ref ";
                node.to->accept(*this);
            }
        }
    }
}

namespace shiranui{
    namespace runtime{
        namespace value{
            // helper functions.
            std::string to_reproductive(sp<Value> vi,sp<syntax::ast::SourceCode> w,bool is_top){
                std::stringstream ss;
                ValueScanner s(w);
                vi->accept(s);
                PrettyPrinterForValue p(ss,s.cnt,w);
                vi->accept(p);
                if(p.found_recursive && is_top){
                    return "<|" + ss.str() + "|>";
                }else{
                    return ss.str();
                }
            }
            // std::string to_reproductive(sp<Value> vi){
            //     return to_reproductive(vi,nullptr);
            // }
        }
    }
}
