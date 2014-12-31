#ifndef VALUE_HPP_INCLUDED
#define VALUE_HPP_INCLUDED

#include <string>
#include <map>
#include "../syntax/ast.hpp"
#include "version.hpp"
#include "change.hpp"

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
        namespace environment{
            struct Environment;
        }
    }
}

namespace shiranui{
    namespace runtime{
        namespace value{
            namespace ast = shiranui::syntax::ast;
            struct Value{
                timemachine::version current_version = 0;
                std::vector<sp<timemachine::ChangeValue> > changes;
                void push_change(sp<timemachine::ChangeValue>);
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
            struct Boolean : Value{
                bool value;
                explicit Boolean(bool);
                void accept(VisitorForValue&);
            };
            struct Array : Value{
                std::vector<sp<Value>> value;
                explicit Array(std::vector<sp<Value>>);
                void accept(VisitorForValue&);
            };
            struct Return : Value{
                sp<Value> value;
                Return(Value*);
                Return(sp<Value>);
                void accept(VisitorForValue&);
            };
            struct Function : Value{
                std::vector<ast::Identifier> parameters;
            };
            struct UserFunction : Function{
                sp<ast::Block> body;
                sp<environment::Environment> env;
                UserFunction(std::vector<ast::Identifier>,sp<ast::Block>,
                             sp<environment::Environment>);
                void accept(VisitorForValue&);
            };
            struct Ref : Value{
                sp<Value> to;
                Ref(sp<Value>);
                void accept(VisitorForValue&);
            };
            struct SystemCall : Function{
                SystemCall();
                void accept(VisitorForValue&);
            };
            struct BuiltinFunction : Function{
                std::string name;
                void accept(VisitorForValue&);
                // if run is fault,return nullptr.
                virtual sp<Value> run(std::vector<sp<Value>>) = 0;
            };
            namespace builtin{
                struct PrintFunction : BuiltinFunction{
                    PrintFunction();
                    sp<Value> run(std::vector<sp<Value>>);
                };
                struct LengthFunction: BuiltinFunction{
                    LengthFunction();
                    sp<Value> run(std::vector<sp<Value>>);
                };
                struct SetIndex : BuiltinFunction{
                    SetIndex();
                    sp<Value> run(std::vector<sp<Value>>);
                };
                struct GetIndex : BuiltinFunction{
                    GetIndex();
                    sp<Value> run(std::vector<sp<Value>>);
                };
            }
            bool check_equality(sp<Value> left,sp<Value> right);
            template<typename T>
            bool is_ref_or_array(T p){
                bool is_ref = std::dynamic_pointer_cast<Ref>(p) != nullptr;
                bool is_array = std::dynamic_pointer_cast<Array>(p) != nullptr;
                return is_ref or is_array;
            }
        }
    }
}

namespace shiranui{
    namespace runtime{
        namespace value{
            struct VisitorForValue{
                virtual ~VisitorForValue(){}
                virtual void visit(Integer&)                      = 0;
                virtual void visit(String&)                       = 0;
                virtual void visit(Boolean&)                      = 0;
                virtual void visit(Array&)                        = 0;
                virtual void visit(UserFunction&)                 = 0;
                virtual void visit(Return&)                       = 0;
                virtual void visit(SystemCall&)                   = 0;
                virtual void visit(BuiltinFunction&)              = 0;
                virtual void visit(Ref&)                          = 0;
            };
        }
    }
}

namespace shiranui{
    namespace runtime{
        namespace timemachine{
            struct SetIndexChange : ChangeValue{
                int index;
                sp<value::Value> prev,next;
                SetIndexChange(int,sp<value::Value>,sp<value::Value>);
                void rollback(sp<value::Value>);
                void flash(sp<value::Value>);
                void rollback(value::Value*);
                void flash(value::Value*);
            };
        }
    }
}

#endif
