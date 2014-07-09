#include "value.hpp"
#include "runner.hpp"

namespace shiranui{
    namespace runtime{
        namespace value{
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
            Return::Return(sp<Value> v) : value(v) {}
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

            void BuiltinFunction::accept(VisitorForValue& v){
                v.visit(*this);
            }

            namespace builtin{
                PrintFunction::PrintFunction(){
                    name = "print";
                    parameters = {ast::Identifier("str")};
                }
                sp<Value> PrintFunction::run(std::vector<sp<Value>> args){
                    if(args.size() != 1){
                        return nullptr;
                    }
                    {
                        sp<String> s = std::dynamic_pointer_cast<String>(args[0]);
                        if(s != nullptr){
                            std::cout << s->value << std::endl;
                            // TODO change to unit
                            return s;
                        }
                    }
                    {
                        sp<Integer> s = std::dynamic_pointer_cast<Integer>(args[0]);
                        if(s != nullptr){
                            std::cout << s->value << std::endl;
                            return s;
                        }
                    }
                    {
                        sp<Boolean> s = std::dynamic_pointer_cast<Boolean>(args[0]);
                        if(s != nullptr){
                            std::cout << (s->value?"true":"false") << std::endl;
                            return s;
                        }
                    }
                    return nullptr;
                }
                LengthFunction::LengthFunction(){
                    name = "len";
                    parameters = {ast::Identifier("array")};
                }
                sp<Value> LengthFunction::run(std::vector<sp<Value>> args){
                    if(args.size() != 1){
                        return nullptr;
                    }
                    {
                        sp<Array> s = std::dynamic_pointer_cast<Array>(args[0]);
                        if(s != nullptr){
                            return std::make_shared<Integer>(s->value.size());
                        }
                    }
                    return nullptr;
                }
            }
        }
    }
}

namespace shiranui{
    namespace runtime{
        namespace value{
            PrettyPrinterForValue::PrettyPrinterForValue(std::ostream& os_)
                : os(os_){
                }
            void PrettyPrinterForValue::visit(Integer& i){
                os << i.value;
            }
            void PrettyPrinterForValue::visit(String& s){
                os << '"' << s.value << '"';
            }
            void PrettyPrinterForValue::visit(Boolean& b){
                os << (b.value?"true":"false");
            }
            void PrettyPrinterForValue::visit(Array& a){
                os << "[";
                for(sp<Value> v : a.value){
                    v->accept(*this);
                    os << ",";
                }
                os << "]";
            }
            void PrettyPrinterForValue::visit(UserFunction& f){
                using shiranui::syntax::ast::PrettyPrinterForAST;
                PrettyPrinterForAST printer_for_ast(os);
                os << "\\(";
                for(size_t i=0;i<f.parameters.size();i++){
                    f.parameters[i].accept(printer_for_ast);
                    if(i != f.parameters.size()-1){
                        os << ",";
                    }
                }
                os << ")" << std::endl;
                f.body->accept(printer_for_ast);
            }
            void PrettyPrinterForValue::visit(Return& r){
                os << "(return ";
                r.value->accept(*this);
                os << ")";
            }

            // builtin
            void PrettyPrinterForValue::visit(SystemCall&){
                os << "system_call";
            }

            void PrettyPrinterForValue::visit(BuiltinFunction& f){
                os << f.name;
            }
        }
    }
}
