#include "value_printer.hpp"
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
                std::map<Value*,int> cnt;
                void visit(Integer& node){cnt[&node]++;}
                void visit(String& node){cnt[&node]++;}
                void visit(Boolean& node){cnt[&node]++;}
                void visit(Array& node){
                    cnt[&node]++;
                    if(cnt[&node] >= 2) return;
                    for(sp<Value> p : node.value){
                        p->accept(*this);
                    }
                }
                void visit(UserFunction& node){cnt[&node]++;}
                void visit(Return& node){cnt[&node]++;}
                void visit(SystemCall& node){cnt[&node]++;}
                void visit(BuiltinFunction& node){cnt[&node]++;}
            };


            PrettyPrinterForValue::PrettyPrinterForValue(std::ostream& os_,
                                                         std::map<Value*,int> c,
                                                         sp<ast::SourceCode> code)
                : os(os_),cur_name("a"),cnt(c),found_recursive(false){
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
                if(where_is_function_from.find(node.body) != where_is_function_from.end()){
                    sp<ast::Function> f = where_is_function_from[node.body];
                    if(f->lambda_id.name.size() == 0){
                        os << "\\(){}";
                    }else{
                        std::stringstream ss;
                        ss << "<|$" << "()" << f->lambda_id.name << "|>";
                        os << ss.str();
                    }
                }else{
                    os << "\\(){}";
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
        }
    }
}

namespace shiranui{
    namespace runtime{
        namespace value{
            // helper functions.
            std::string to_reproductive(sp<Value> vi,sp<syntax::ast::SourceCode> w){
                std::stringstream ss;
                ValueScanner s;
                vi->accept(s);
                PrettyPrinterForValue p(ss,s.cnt,w);
                vi->accept(p);
                if(p.found_recursive){
                    return "<|" + ss.str() + "|>";
                }else{
                    return ss.str();
                }
            }
            std::string to_reproductive(sp<Value> vi){
                return to_reproductive(vi,nullptr);
            }
        }
    }
}
