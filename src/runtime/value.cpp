#include "value.hpp"
#include "runner.hpp"
#include "value_printer.hpp"

#include <sstream>
#include <set>

namespace shiranui{
    namespace runtime{
        namespace value{
            void Value::push_change(sp<timemachine::ChangeValue> c){
                changes.push_back(c);
                current_version++;
            }
            Integer::Integer(int v) : value(v) {};
            void Integer::accept(VisitorForValue& v){
                v.visit(*this);
            }
            // String
            String::String(std::string v) : value(v) {}
            void String::accept(VisitorForValue& v){
                v.visit(*this);
            }
            Boolean::Boolean(bool v) : value(v) {}
            void Boolean::accept(VisitorForValue& v){
                v.visit(*this);
            }

            Array::Array(std::vector<sp<Value>> v) : value(v) {}
            void Array::accept(VisitorForValue& v){
                v.visit(*this);
            }


            // Return
            Return::Return(Value* v) : value(v) {}
            void Return::accept(VisitorForValue& v){
                v.visit(*this);
            }
            // Function
            UserFunction::UserFunction(std::vector<ast::Identifier> ps,
                                       sp<ast::Block> b,
                                       sp<environment::Environment> e)
                : body(b),env(e){
                parameters = ps;
            }
            void UserFunction::accept(VisitorForValue& v){
                v.visit(*this);
            }

            SystemCall::SystemCall(){
                parameters = {ast::Identifier("str")};
            }
            void SystemCall::accept(VisitorForValue& v){
                v.visit(*this);
            }


            BuiltinFunction::BuiltinFunction(Memory* memory_)
                : memory(memory_) {}
            void BuiltinFunction::accept(VisitorForValue& v){
                v.visit(*this);
            }

            Ref::Ref(sp<Value> p) : to(p) {}
            void Ref::accept(VisitorForValue& v){
                v.visit(*this);
            }
            namespace builtin{
                PrintFunction::PrintFunction(Memory* memory) : BuiltinFunction(memory){
                    name = "print";
                    parameters = {ast::Identifier("str")};
                }
                sp<Value> PrintFunction::run(std::vector<sp<Value>> args){
                    if(args.size() != 1){
                        return nullptr;
                    }
                    {
                        sp<String> s = dynamic_cast<String*>(args[0]);
                        if(s != nullptr){
                            std::cout << s->value << std::endl;
                            // TODO change to unit
                            return s;
                        }
                    }
                    {
                        sp<Integer> s = dynamic_cast<Integer*>(args[0]);
                        if(s != nullptr){
                            std::cout << s->value << std::endl;
                            return s;
                        }
                    }
                    {
                        sp<Boolean> s = dynamic_cast<Boolean*>(args[0]);
                        if(s != nullptr){
                            std::cout << (s->value?"true":"false") << std::endl;
                            return s;
                        }
                    }
                    return nullptr;
                }
                LengthFunction::LengthFunction(Memory* memory) : BuiltinFunction(memory){
                    name = "len";
                    parameters = {ast::Identifier("array")};
                }
                sp<Value> LengthFunction::run(std::vector<sp<Value>> args){
                    if(args.size() != 1){
                        return nullptr;
                    }
                    {
                        sp<Array> s = dynamic_cast<Array*>(args[0]);
                        if(s != nullptr){
                            return memory->create<Integer>(s->value.size());
                        }
                    }
                    {
                        auto s = dynamic_cast<String*>(args[0]);
                        if(s != nullptr){
                            return memory->create<Integer>(s->value.size());
                        }
                    }
                    return nullptr;
                }
                SetIndex::SetIndex(Memory* memory) : BuiltinFunction(memory){
                    name = "set";
                    parameters = {ast::Identifier{"array"},
                                  ast::Identifier{"index"},
                                  ast::Identifier{"value"}};
                }
                sp<Value> SetIndex::run(std::vector<sp<Value>> args){
                    if(args.size() != 3) return nullptr;
                    {
                        sp<Array> array = dynamic_cast<Array*>(args[0]);
                        sp<Integer> index = dynamic_cast<Integer*>(args[1]);
                        if(array == nullptr or index == nullptr){
                            return nullptr;
                        }
                        if(0 <= index->value and index->value < array->value.size()){
                            array->push_change(memory->create<timemachine::SetIndexChange>(index->value,
                                                                                           array->value[index->value],
                                                                                           args[2]));
                            // array->push_change(std::make_shared<timemachine::SetIndexChange>(index->value,
                            //                                                                  array->value[index->value],
                            //                                                                  args[2]));
                            array->value[index->value] = args[2];

                            return array;
                        }else{
                            return nullptr;
                        }
                    }
                }
                GetIndex::GetIndex(Memory* memory) : BuiltinFunction(memory){
                    name = "get";
                    parameters = {ast::Identifier{"array"},
                                  ast::Identifier{"index"}};
                }
                sp<Value> GetIndex::run(std::vector<sp<Value>> args){
                    if(args.size() != 2) return nullptr;
                    {
                        sp<Array> array = dynamic_cast<Array*>(args[0]);
                        sp<Integer> index = dynamic_cast<Integer*>(args[1]);
                        if(array != nullptr and index != nullptr){
                            if(0 <= index->value and index->value < array->value.size()){
                                return array->value[index->value];
                            }else{
                                // throw exception
                                return nullptr;
                            }
                        }
                    }
                    {
                        auto string = dynamic_cast<String*>(args[0]);
                        sp<Integer> index = dynamic_cast<Integer*>(args[1]);
                        if(string != nullptr and index != nullptr){
                            if(0 <= index->value and index->value < string->value.size()){
                                std::stringstream ss;
                                ss << string->value[index->value];
                                return memory->create<String>(ss.str());
                            }else{
                                return nullptr;
                            }
                        }
                    }
                    return nullptr;
                }
            }
        }
    }
}



namespace shiranui {
    namespace runtime {
        namespace value {
            bool check_equality(sp<Value> left,sp<Value> right,std::set<std::set<sp<Value> > >& checked){
                if(typeid(*left) != typeid(*right)){
                    return false;
                }
                // TODO: show correctness
                if(checked.find({left,right}) != checked.end()){
                    return true;
                }
                checked.insert({left,right});
                {
                    auto l = dynamic_cast<Integer*>(left);
                    auto r = dynamic_cast<Integer*>(right);
                    if(l != nullptr and r != nullptr){
                        return l->value == r->value;
                    }
                }
                {
                    auto l = dynamic_cast<Boolean*>(left);
                    auto r = dynamic_cast<Boolean*>(right);
                    if(l != nullptr and r != nullptr){
                        return l->value == r->value;
                    }
                }
                {
                    auto l = dynamic_cast<String*>(left);
                    auto r = dynamic_cast<String*>(right);
                    if(l != nullptr and r != nullptr){
                        return l->value == r->value;
                    }
                }
                {
                    auto l = dynamic_cast<Array*>(left);
                    auto r = dynamic_cast<Array*>(right);
                    if(l != nullptr and r != nullptr){
                        if(l->value.size() != r->value.size()){
                            return false;
                        }else{
                            bool ok = true;
                            for(size_t i=0;i<l->value.size();i++){
                                ok = ok and check_equality(l->value[i],r->value[i],checked);
                            }
                            return ok;
                        }
                    }
                }
                return false;
            }
            bool check_equality(sp<Value> left, sp<Value> right){
                std::set<std::set<sp<Value> > > checked;
                return check_equality(left,right,checked);
            }
            bool is_ref_or_array(Value* p){
                bool is_ref = dynamic_cast<Ref*>(p) != nullptr;
                bool is_array = dynamic_cast<Array*>(p) != nullptr;
                return is_ref or is_array;
            }
            bool is_userfunction(sp<Value> p){
                bool is_uf = dynamic_cast<UserFunction*>(p) != nullptr;
                return is_uf;
            }
        }
    }
}

namespace shiranui{
    namespace runtime{
        namespace timemachine{
            using namespace value;
            SetIndexChange::SetIndexChange(int i,sp<Value> p,sp<Value> n)
                : index(i),prev(p),next(n) {}

            void SetIndexChange::rollback(Value* target_){
                auto target = dynamic_cast<Array*>(target_);
                if(target == nullptr){
                    throw TimeMachineException();
                }
                target->value[index] = prev;
            }
            void SetIndexChange::flash(Value* target_){
                auto target = dynamic_cast<Array*>(target_);
                if(target == nullptr){
                    throw TimeMachineException();
                }
                target->value[index] = next;
            }

            RefChange::RefChange(sp<Value> p,sp<Value> n)
                : prev(p),next(n) {}

            void RefChange::rollback(Value* target_){
                auto target = dynamic_cast<Ref*>(target_);
                if(target == nullptr){
                    throw TimeMachineException();
                }
                target->to = prev;
            }
            void RefChange::flash(Value* target_){
                auto target = dynamic_cast<Ref*>(target_);
                if(target == nullptr){
                    throw TimeMachineException();
                }
                target->to = next;
            }
        }
    }
}

