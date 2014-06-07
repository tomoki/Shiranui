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
            struct VisitorForValue;
        }
    }
}
namespace shiranui{
    namespace runtime{
        namespace value{
            namespace ast = shiranui::syntax::ast;
            struct Value{
                virtual ~Value() {};
                virtual void accept(VisitorForValue&) = 0;
            };
            struct Integer : Value{
                int value;
                explicit Integer(int v);
                void accept(VisitorForValue&);
            };
            struct String : Value{
                std::string value;
                explicit String(std::string);
                void accept(VisitorForValue&);
            };
            struct Function : Value{
                std::vector<ast::Identifier> parameters;
            };
            struct UserFunction : Function{
                sp<ast::Block> body;
                UserFunction(std::vector<ast::Identifier>,sp<ast::Block>);
                void accept(VisitorForValue&);
            };
            struct Return : Value{
                sp<Value> value;
                Return(Value*);
                Return(sp<Value>);
                void accept(VisitorForValue&);
            };
        }
    }
}

namespace shiranui{
    namespace runtime{
        namespace value{
            struct VisitorForValue{
                virtual ~VisitorForValue(){}
                virtual void visit(Integer&)      = 0;
                virtual void visit(String&)       = 0;
                virtual void visit(UserFunction&) = 0;
                virtual void visit(Return&)       = 0;
            };
            struct PrettyPrinterForValue : VisitorForValue{
                std::ostream& os;
                PrettyPrinterForValue(std::ostream& os);
                void visit(Integer&);
                void visit(String&);
                void visit(UserFunction&);
                void visit(Return&);
            };
        }
    }
}

#endif
