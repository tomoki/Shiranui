#ifndef DIVER_HPP_INCLUDED
#define DIVER_HPP_INCLUDED

#include "diving_message.hpp"
#include "../../syntax/ast.hpp"
#include <string>
#include <stack>
#include <utility>

namespace shiranui{
    namespace runtime{
        namespace diver{
            struct Diver{
                sp<syntax::ast::SourceCode> source;
                int current_id;
                std::stack<std::pair<int,sp<syntax::ast::Expression>>> undo_stack;
                Diver(sp<syntax::ast::SourceCode>);

                DivingMessage clear();
                DivingMessage dive(int);
                DivingMessage dive(sp<syntax::ast::Expression>);
                DivingMessage dive(syntax::ast::FunctionCall&);
                DivingMessage dive(sp<syntax::ast::Expression>,int);
                DivingMessage dive(syntax::ast::FunctionCall&,int);
                DivingMessage see(syntax::ast::Block&,int);
                DivingMessage surface(); // undo
                DivingMessage scan_flymark(syntax::ast::SourceCode&);
            };
        }
    }
}
#endif
