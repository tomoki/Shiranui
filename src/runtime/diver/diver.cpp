#include "diver.hpp"
#include "swimfin.hpp"
#include "snorkel.hpp"
#include "harpoon.hpp"
#include "jumper.hpp"
#include "../runner.hpp"
#include <sstream>

namespace shiranui{
    namespace runtime{
        namespace diver{
            using namespace syntax::ast;
            using namespace infomation;
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
                undo_stack = decltype(undo_stack)();
                current_id = infomation::TOPLEVEL;
                return DivingMessage();
            }

            DivingMessage Diver::dive(sp<syntax::ast::Expression> exp){
                return dive(exp,current_id);
            }

            DivingMessage Diver::dive(sp<syntax::ast::Expression> exp,int lower_id){
                using namespace shiranui::syntax::ast;
                using namespace shiranui::runtime::value;
                auto next_id = return_value(*exp,lower_id).first;
                {
                    auto p = std::dynamic_pointer_cast<FunctionCall>(exp);
                    if(p != nullptr){
                        auto pp = return_value(*p->function,lower_id);
                        auto uf = std::dynamic_pointer_cast<UserFunction>(pp.second);
                        if(uf == nullptr){
                            DivingMessage d;
                            d.add_error(*p,"Not called");
                            return d;
                        }
                        return dive(uf->body,next_id);
                    }
                }
                DivingMessage d;
                d.add_error(*exp,"It is not functioncall");
                return d;
            }
            DivingMessage Diver::dive(sp<syntax::ast::Block> block,int call_under){
                using namespace shiranui::syntax::ast;
                using namespace shiranui::runtime::value;
                auto message = see(*block,call_under);
                current_id = call_under;
                undo_stack.push(std::make_pair(call_under,block));
                return message;
            }

            DivingMessage Diver::scan_flymark(sp<syntax::ast::SourceCode> source){
                return use_harpoon(source);
            }
            DivingMessage Diver::see(syntax::ast::Block& block,int call_under){
                lift_candidate = get_lift_candidate(source,block,call_under);
                return use_snorkel(source,block,call_under);
            }
            DivingMessage Diver::surface(){
                if(undo_stack.empty()){
                    return DivingMessage();
                }else{
                    // remove current state
                    auto current_t = undo_stack.top();
                    undo_stack.pop();
                    // maybe toplevel
                    if(undo_stack.empty()){
                        return dive(current_t.second,current_t.first);
                    }
                    auto p = undo_stack.top();
                    auto d = dive(p.second,p.first);
                    // dive push new state.It should be removed.
                    //  for multiple back
                    undo_stack.pop();
                    return d;
                }
            }
            DivingMessage Diver::move_to_caller(){
                if(undo_stack.empty()) return DivingMessage();
                auto current = undo_stack.top();
                auto parent = current.second->runtime_info.get_up(current.first);
                if(parent.first >= 0 and parent.second != nullptr){
                    return dive(parent.second,parent.first);
                }else{
                    return DivingMessage();
                }
            }
            DivingMessage Diver::jump(int point,int index){
                auto p = use_jumper(source,point,index);
                auto block = p.second;
                if(block == nullptr) return DivingMessage();
                int call_under = p.first;
                current_id = call_under;
                return dive(block,call_under);
            }
            DivingMessage Diver::lift(int from,int to){
                DivingMessage m;
                for(auto p : lift_candidate){
                    if(p->point == from and p->length == (to-from)){
                        auto f = to_reproductive(return_value(*p->function,current_id).second,source);
                        std::vector<std::string> args;
                        for(auto a : p->arguments){
                            args.push_back(to_reproductive(return_value(*a,current_id).second,source));
                        }
                        std::string ret = f;
                        ret += "(";
                        for(size_t i=0;i<args.size();i++){
                            ret += args[i];
                            if(i+1 != args.size()){
                                ret += ",";
                            }
                        }
                        ret += ")";
                        m.add_lift_result(ret);
                    }
                }
                return m;
            }
        }
    }
}
