#include "diver.hpp"
#include "../runner.hpp"
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
                : source(source_),current_id(0){
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
                return DivingMessage();
            }
            DivingMessage Diver::dive(syntax::ast::FunctionCall& fc){
                return dive(fc,current_id);
            }
            DivingMessage Diver::dive(syntax::ast::FunctionCall& fc,int lower_id){
                using namespace shiranui::syntax::ast;
                using namespace shiranui::runtime::value;
                auto p = return_value(*fc.function,lower_id);
                auto function = std::dynamic_pointer_cast<UserFunction>(p.second);
                auto message = see(*function->body,p.first);

                current_id = return_value(fc,lower_id).first;
                undo_stack.push(std::make_pair(current_id,
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
            SwimFin::SwimFin(int point_)
                : treasure(nullptr),point(point_){
            }
            void SwimFin::visit(Identifier& node){
                throw InternalException(std::make_shared<Identifier>(node));
            }
            void SwimFin::visit(Variable& node){
                if(not in_range(node)) return;
            }
            void SwimFin::visit(Number& node){
                if(not in_range(node)) return;
            }
            void SwimFin::visit(String& node){
                if(not in_range(node)) return;
            }
            void SwimFin::visit(Enum& node){
                if(not in_range(node)) return;
                for(sp<Expression> e : node.expressions){
                    e->accept(*this);
                }
            }
            void SwimFin::visit(Interval& node){
                if(not in_range(node)) return;
                if(node.start != nullptr){
                    node.start->accept(*this);
                }
                if(node.end != nullptr){
                    node.end->accept(*this);
                }
                if(node.next != nullptr){
                    node.next->accept(*this);
                }
            }
            void SwimFin::visit(Block& node){
                if(not in_range(node)) return;
                for(sp<Statement> s : node.statements){
                    s->accept(*this);
                }
            }
            void SwimFin::visit(Function& node){
                if(not in_range(node)) return;
                node.body->accept(*this);
            }
            void SwimFin::visit(FunctionCall& node){
                if(not in_range(node)) return;
                treasure = std::make_shared<FunctionCall>(node);
                node.function->accept(*this);
                for(sp<Expression> a : node.arguments){
                    a->accept(*this);
                }
            }
            void SwimFin::visit(BinaryOperator& node){
                if(not in_range(node)) return;
                node.left->accept(*this);
                node.right->accept(*this);
            }
            void SwimFin::visit(UnaryOperator& node){
                if(not in_range(node)) return;
                node.exp->accept(*this);
            }
            void SwimFin::visit(IfElseExpression& node){
                if(not in_range(node)) return;
                node.pred->accept(*this);
                node.ife->accept(*this);
                node.elsee->accept(*this);
            }
            void SwimFin::visit(Definement& node){
                if(not in_range(node)) return;
                node.value->accept(*this);
            }
            void SwimFin::visit(ReturnStatement& node){
                if(not in_range(node)) return;
                node.value->accept(*this);
            }
            void SwimFin::visit(IfElseStatement& node){
                if(not in_range(node)) return;
                node.pred->accept(*this);
                node.ifblock->accept(*this);
                node.elseblock->accept(*this);
            }
            // should return forstatement?
            void SwimFin::visit(ForStatement& node){
                if(not in_range(node)) return;
                node.loop_exp->accept(*this);
                node.block->accept(*this);
            }
            void SwimFin::visit(Assignment& node){
                if(not in_range(node)) return;
                node.value->accept(*this);
            }
            void SwimFin::visit(TestFlyLine& node){
                if(not in_range(node)) return;
                throw InternalException(std::make_shared<TestFlyLine>(node));
            }
            void SwimFin::visit(IdleFlyLine& node){
                if(not in_range(node)) return;
                throw InternalException(std::make_shared<IdleFlyLine>(node));
            }
            void SwimFin::visit(SourceCode& node){
                if(not in_range(node)) return;
                for(sp<Statement> s : node.statements){
                    s->accept(*this);
                }
            }
            template<typename T>
            bool SwimFin::in_range(T& t){
                int start_point = t.point;
                int end_point = start_point + t.length;
                return start_point <= point and point <= end_point;
            }
            template<typename T>
            bool SwimFin::in_range(sp<T> t){
                int start_point = t->point;
                int end_point = start_point + t->length;
                return start_point <= point and point <= end_point;
            }

            sp<syntax::ast::LocationInfo> use_swimfin(SourceCode& sc,int point){
                SwimFin sf(point);
                sf.visit(sc);
                return sf.treasure; // can be
            }
        }
    }
}

