#include "value.hpp"

namespace shiranui{
    namespace runtime{
        namespace value{
            // Integer
            Integer::Integer(int v) : value(v) {};
            std::ostream& Integer::serialize(std::ostream& os) {
                return os << value;
            }

            // String
            String::String(std::string v) : value(v) {}
            std::ostream& String::serialize(std::ostream& os) {
                return os << value;
            }

            // Function
            Function::Function(std::vector<ast::Identifier> ps,
                               ast::Block*                  b)
                : parameters(ps),body(b) {}
            Function::Function(std::vector<ast::Identifier> ps,
                               sp<ast::Block>               b)
                : parameters(ps),body(b) {}

            std::ostream& Function::serialize(std::ostream &os) {
                os << "\\(";
                for(size_t i=0;i<parameters.size();i++){
                    os << parameters[i];
                    if(i != parameters.size()-1){
                        os << ",";
                    }
                }
                os << ")" << std::endl;
                os << *body;
                return os ;
            }

            // Return
            Return::Return(Value* v) : value(v) {}
            std::ostream& Return::serialize(std::ostream& os) {
                return os << "return " << *value;
            }
            // Value
            std::ostream& operator<<(std::ostream& os, Value& s){
                return s.serialize(os);
            }
        }
    }
}
