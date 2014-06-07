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

            // Function
            UserFunction::UserFunction(std::vector<ast::Identifier> ps,
                                       sp<ast::Block>               b)
                : body(b) {
                    parameters = ps;
            }
            void UserFunction::accept(VisitorForValue& v){
                v.visit(*this);
            }

            // Return
            Return::Return(Value* v) : value(v) {}
            Return::Return(sp<Value> v) : value(v) {}
            void Return::accept(VisitorForValue& v){
                v.visit(*this);
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
        }
    }
}
