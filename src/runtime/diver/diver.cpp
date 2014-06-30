#include "diver.hpp"
#include <sstream>

namespace shiranui{
    namespace runtime{
        namespace diver{
            template<typename T>
            std::pair<int,sp<runtime::value::Value>> return_value(T& ast_node,int lower_id){
                auto it = std::lower_bound(ast_node.runtime_info.visit_time.begin(),
                                           ast_node.runtime_info.visit_time.end(),
                                           lower_id);
                if(it == ast_node.runtime_info.visit_time.end()){
                    return std::make_pair(-1,nullptr);
                }else{
                    return std::make_pair(*it,ast_node.runtime_info.return_value[*it]);
                }
            }

            Diver::Diver(sp<syntax::ast::SourceCode> source_)
                : source(source_){
            }
            DivingMessage Diver::dive(sp<syntax::ast::Expression> exp,int lower_id){
                using namespace shiranui::syntax::ast;
                {
                    auto p = std::dynamic_pointer_cast<FunctionCall>(exp);
                    if(p != nullptr){
                        return dive(*p,lower_id);
                    }
                }
                return DivingMessage();
            }

            DivingMessage Diver::dive(syntax::ast::FunctionCall& fc,int lower_id){
                using namespace shiranui::syntax::ast;
                using namespace shiranui::runtime::value;
                auto p = return_value(*fc.function,lower_id);
                auto function = std::dynamic_pointer_cast<UserFunction>(p.second);
                auto message = see(*function->body,p.first);

                // calc return_value for id.
                undo_stack.push(std::make_pair(return_value(fc,lower_id).first,
                                           std::make_shared<FunctionCall>(fc)));
                return message;
            }
            DivingMessage Diver::see(syntax::ast::Block& block,int lower_id){
                using namespace shiranui::syntax::ast;
                using namespace shiranui::runtime::value;

                DivingMessage message;
                for(sp<Statement> s : block.statements){
                    {
                        auto p = std::dynamic_pointer_cast<IfElseStatement>(s);
                        if(p != nullptr){
                            auto if_p = return_value(*p->ifblock,lower_id);
                            auto else_p = return_value(*p->elseblock,lower_id);
                            if(if_p.second == nullptr and else_p.second == nullptr){
                                //error.
                            }else{
                                bool if_called = (if_p.second != nullptr
                                                   and else_p.second != nullptr
                                                   and if_p.first < else_p.first)
                                                  or (else_p.second == nullptr);
                                if(if_called){
                                    message.add_strike(*p->elseblock);
                                    message = message + see(*p->ifblock,if_p.first);
                                }else{
                                    message.add_strike(*p->ifblock);
                                    message = message + see(*p->elseblock,else_p.first);
                                }
                            }
                        }
                    }
                }
                return message;
            }
            DivingMessage Diver::undo(){
                if(undo_stack.empty()){
                    return DivingMessage();
                }else{
                    // remove current state
                    undo_stack.pop();
                    std::pair<int,sp<syntax::ast::Expression>> p = undo_stack.top();
                    return dive(p.second,p.first);
                }
            }

            std::string DivingMessage::str(){
                return cache;
            }
            template<typename T>
            DivingMessage DivingMessage::add_strike(const T& t){
                std::stringstream ss;
                ss << cache << STRIKE << std::endl
                   << t.point << " " << t.length << std::endl;
                cache = ss.str();
                return *this;
            }
            DivingMessage DivingMessage::operator+(DivingMessage message){
                DivingMessage ret;
                ret.cache = cache + message.cache;
                return ret;
            }

            using namespace syntax::ast;
            // SwimFin
            // void SwimFin::visit(
            SwimFin::SwimFin()
                : treasure(nullptr){
            }
            void SwimFin::visit(Identifier& node){
            }
            void SwimFin::visit(Variable& node){
            }
            void SwimFin::visit(Number& node){
            }
            void SwimFin::visit(String& node){
            }
            void SwimFin::visit(Enum& node){
            }
            void SwimFin::visit(Interval& node){
            }
            void SwimFin::visit(Block& node){
            }
            void SwimFin::visit(Function& node){
            }
            void SwimFin::visit(FunctionCall& node){
                // calc point.
                //return std::make_shared<FunctionCall>(node);
            }
            void SwimFin::visit(BinaryOperator& node){
            }
            void SwimFin::visit(UnaryOperator& node){
            }
            void SwimFin::visit(IfElseExpression& node){
            }
            void SwimFin::visit(Definement& node){
            }
            void SwimFin::visit(ReturnStatement& node){
            }
            void SwimFin::visit(IfElseStatement& node){
            }
            void SwimFin::visit(ForStatement& node){
            }
            void SwimFin::visit(Assignment& node){
            }
            void SwimFin::visit(TestFlyLine& node){
            }
            void SwimFin::visit(IdleFlyLine& node){
            }
            void SwimFin::visit(SourceCode& node){
            }
        }
    }
}

