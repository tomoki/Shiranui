#ifndef DIVER_HPP_INCLUDED
#define DIVER_HPP_INCLUDED

#include "../syntax/ast.hpp"
#include <string>
#include <stack>
#include <utility>

namespace shiranui{
    namespace runtime{
        namespace diver{
            const std::string STRIKE = "strike";
            struct DivingMessage{
                std::string cache;
                std::string str();
                template<typename T>
                DivingMessage add_strike(const T&);
                DivingMessage operator+(DivingMessage);
            };

            struct Diver{
                sp<syntax::ast::SourceCode> source;
                std::stack<std::pair<int,sp<syntax::ast::Expression>>> undo_stack;
                Diver(sp<syntax::ast::SourceCode>);
                DivingMessage dive(sp<syntax::ast::Expression>,int=0);
                DivingMessage dive(syntax::ast::FunctionCall&,int=0);
                DivingMessage see(syntax::ast::Block&,int);
                DivingMessage undo();
            };
        }
    }
}
#endif
