#include "value.hpp"

namespace shiranui{
    namespace value{
        // Integer
        Integer::Integer(int v) : value(v) {};
        std::ostream& Integer::serialize(std::ostream& os) const{
            return os << value;
        }

        // String
        String::String(std::string v) : value(v) {}
        std::ostream& String::serialize(std::ostream& os) const{
            return os << value;
        }

        // Function
        Function::Function(std::vector<ast::Identifier> ps,
                           ast::Block*                  b)
            : parameters(ps),body(b) {}
        std::ostream& Function::serialize(std::ostream &os) const{
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
        Return::Return(Value* v)
            : value(v) {}
        std::ostream& Return::serialize(std::ostream& os) const{
            return os << "return " << *value;
        }
        // Value
        std::ostream& operator<<(std::ostream& os, const Value& s){
            return s.serialize(os);
        }
    }
}
