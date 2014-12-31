#ifndef VALUE_PRINTER_HPP
#define VALUE_PRINTER_HPP

#include "value.hpp"
#include <exception>

namespace shiranui{
    namespace runtime{
        namespace value{
            struct ValuePrinterException : std::exception{
            };
        }
    }
}
namespace shiranui{
    namespace runtime{
        namespace value{
            std::string to_reproductive(sp<runtime::value::Value>,
                                        sp<syntax::ast::SourceCode> w=nullptr);
        }
    }
}

#endif
