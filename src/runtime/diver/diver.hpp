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
                std::vector<sp<syntax::ast::FunctionCall>> lift_candidate;
                int current_id;
                std::stack<std::pair<int,sp<syntax::ast::Block>>> undo_stack;
                Diver(sp<syntax::ast::SourceCode>);

                DivingMessage clear();
                DivingMessage dive(int);
                DivingMessage dive(sp<syntax::ast::Expression>);
                DivingMessage dive(sp<syntax::ast::Expression>,int);
                DivingMessage dive(sp<syntax::ast::Block>,int);
                DivingMessage see(syntax::ast::Block&,int);
                // point,flymark_index
                DivingMessage jump(int,int);
                DivingMessage surface(); // undo
                DivingMessage move_to_caller();
                DivingMessage scan_flymark(sp<syntax::ast::SourceCode>);
                DivingMessage lift(int,int);
            };
        }
    }
}
#endif
