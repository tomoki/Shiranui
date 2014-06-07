#ifndef VALUE_HPP_INCLUDED
#define VALUE_HPP_INCLUDED

#include <string>
#include "../syntax/ast.hpp"
namespace shiranui{
    namespace syntax{
        namespace ast{
            struct LocationInfo;
            struct Identifier;
            struct Block;
        }
    }
}
namespace shiranui{
    namespace runtime{
        namespace value{
            namespace ast = shiranui::syntax::ast;
            struct Value{
                virtual ~Value() {};
                virtual std::ostream& serialize(std::ostream&)  = 0;
            };
            struct Integer : Value{
                int value;
                explicit Integer(int v);
                std::ostream& serialize(std::ostream&);
            };
            struct String : Value{
                std::string value;
                explicit String(std::string);
                std::ostream& serialize(std::ostream&);
            };
            struct Function : Value{
                std::vector<ast::Identifier> parameters;
            };
            struct UserFunction : Function{
                sp<ast::Block> body;
                UserFunction(std::vector<ast::Identifier>,sp<ast::Block>);
                std::ostream& serialize(std::ostream&);
            };
//            struct BuiltinFunction : Function{
//                // virtual apply(Runner& r,std::vector<ast::Expression> arguments) = ;
//                std::ostream& serialize(std::ostream&);
//            };
            struct Return : Value{
                sp<Value> value;
                Return(Value*);
                Return(sp<Value>);
                std::ostream& serialize(std::ostream&);
            };
        }
    }
}
namespace shiranui{
    namespace runtime{
        namespace value{
            std::ostream& operator<<(std::ostream&,Value&);
        }
    }
}

#endif
