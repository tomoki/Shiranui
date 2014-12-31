#ifndef VALUE_PRINTER_HPP
#define VALUE_PRINTER_HPP

#include "value.hpp"

namespace shiranui{
    namespace runtime{
        namespace value{
            std::string to_reproductive(sp<runtime::value::Value>,
                                        sp<syntax::ast::SourceCode> w=nullptr,
                                        bool is_top=true);
        }
    }
}


namespace shiranui{
    namespace runtime{
        namespace value{
            struct PrettyPrinterForValue : VisitorForValue{
                std::ostream& os;
                std::string cur_name;
                std::map<Value*,int> cnt;
                std::map<Value*,std::string> name;
                bool found_recursive;
                sp<ast::SourceCode> code;
                std::map<sp<ast::Block>,sp<ast::Function>> where_is_function_from;
                PrettyPrinterForValue(std::ostream& os,std::map<Value*,int>,
                                      sp<ast::SourceCode>);
                bool already_appeared(Value*);
                void use_prev_name(Value*);
                void register_name(Value*);
                bool check_already_occured(Value*);
                void visit(Integer&);
                void visit(String&);
                void visit(Boolean&);
                void visit(Array&);
                void visit(UserFunction&);
                void visit(Return&);
                void visit(SystemCall&);
                void visit(BuiltinFunction&);
                void visit(Ref&);
            };
        }
    }
}

#endif
