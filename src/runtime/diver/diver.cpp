#include "diver.hpp"
#include "swimfin.hpp"
#include "snorkel.hpp"
#include "harpoon.hpp"
#include "../runner.hpp"
#include <sstream>

namespace shiranui{
    namespace runtime{
        namespace diver{
            using namespace syntax::ast;
            Diver::Diver(sp<syntax::ast::SourceCode> source_)
                : source(source_),current_id(infomation::TOPLEVEL){
            }

            // TODO: should treat for.
            DivingMessage Diver::dive(int point){
                using namespace shiranui::syntax::ast;
                auto exp_or_for = use_swimfin(*source,point);
                {
                    auto p = std::dynamic_pointer_cast<Expression>(exp_or_for);
                    if(p != nullptr){
                        return dive(p);
                    }
                }
                return DivingMessage();
            }

            DivingMessage Diver::clear(){
                undo_stack = std::stack<std::pair<int,sp<syntax::ast::Expression>>>();
                current_id = infomation::TOPLEVEL;
                return DivingMessage();
            }

            DivingMessage Diver::dive(sp<syntax::ast::Expression> exp){
                return dive(exp,current_id);
            }

            DivingMessage Diver::dive(sp<syntax::ast::Expression> exp,int lower_id){
                using namespace shiranui::syntax::ast;
                {
                    auto p = std::dynamic_pointer_cast<FunctionCall>(exp);
                    if(p != nullptr){
                        return dive(*p,lower_id);
                    }
                }
                DivingMessage d;
                d.add_error(*exp,"It is not functioncall");
                return d;
            }
            DivingMessage Diver::dive(syntax::ast::FunctionCall& fc){
                return dive(fc,current_id);
            }
            DivingMessage Diver::dive(syntax::ast::FunctionCall& fc,int call_under){
                using namespace shiranui::syntax::ast;
                using namespace shiranui::runtime::value;
                auto ret = return_value(fc,call_under);
                auto p = return_value(*fc.function,call_under);
                auto function = std::dynamic_pointer_cast<UserFunction>(p.second);
                if(p.second == nullptr){
                    // send error
                    DivingMessage m;
                    m.add_error(fc,"Not called function");
                    return m;
                }
                // ret.first is new call_under
                auto message = see(*function->body,ret.first);

                current_id = ret.first;
                undo_stack.push(std::make_pair(call_under,
                                               std::make_shared<FunctionCall>(fc)));
                return message;
            }

            DivingMessage Diver::scan_flymark(syntax::ast::SourceCode& source){
                return use_harpoon(source);
            }
            DivingMessage Diver::see(syntax::ast::Block& block,int call_under){
                return use_snorkel(block,call_under);
            }
            DivingMessage Diver::surface(){
                if(undo_stack.empty()){
                    return DivingMessage();
                }else{
                    // remove current state
                    undo_stack.pop();
                    std::pair<int,sp<syntax::ast::Expression>> p = undo_stack.top();
                    return dive(p.second,p.first);
                }
            }
        }
    }
}
