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
    namespace value{
        namespace ast = shiranui::syntax::ast;
        struct Value{
            virtual ~Value() {};
            virtual std::ostream& serialize(std::ostream&) const = 0;
        };
        struct Integer : Value{
            int value;
            explicit Integer(int v);
            std::ostream& serialize(std::ostream& os) const;
        };
        struct String : Value{
            std::string value;
            explicit String(std::string v);
            std::ostream& serialize(std::ostream& os) const;
        };
        struct Function : Value{
            std::vector<ast::Identifier> parameters;
            sp<ast::Block>               body;
            Function(std::vector<ast::Identifier> ps,ast::Block* b);
            std::ostream& serialize(std::ostream &os) const;
        };
        struct Return : Value{
            sp<Value> value;
            Return(Value* v);
            std::ostream& serialize(std::ostream& os) const;
        };
    }
}
namespace shiranui{
    namespace value{
        std::ostream& operator<<(std::ostream& os, const Value& s);
    }
}

#endif
