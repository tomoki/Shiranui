#include "diver.hpp"
#include "../runner.hpp"
#include <sstream>

namespace shiranui{
    namespace runtime{
        namespace diver{
            template<typename T>
            std::pair<int,sp<runtime::value::Value>> return_value(T& ast_node,
                                                                  int call_under_id){
                if(ast_node.runtime_info.call_under.find(call_under_id)
                   != ast_node.runtime_info.call_under.end()){
                    int id = ast_node.runtime_info.call_under[call_under_id];
                    return std::make_pair(id,ast_node.runtime_info.return_value[id]);
                }else{
                    return std::make_pair(-2,nullptr);
                }
            }

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
                return DivingMessage();
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
                // ret.first is new call_under
                auto message = see(*function->body,ret.first);

                current_id = ret.first;
                undo_stack.push(std::make_pair(call_under,
                                               std::make_shared<FunctionCall>(fc)));
                return message;
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
            template<typename T>
            DivingMessage DivingMessage::add_explore(const T& t,const std::string& what){
                std::stringstream ss;
                ss << cache << EXPLORE << std::endl
                   << t.point << " " << t.length << std::endl
                   << what << std::endl;
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

            template<typename T>
            sp<syntax::ast::LocationInfo> use_swimfin(T& sc,int point){
                SwimFin sf(point);
                sf.visit(sc);
                return sf.treasure; // can be null
            }
            template<typename T>
            DivingMessage use_snorkel(T& block,int call_under){
                Snorkel s(call_under);
                s.visit(block);
                return s.message;
            }


            // Snorkel
            Snorkel::Snorkel(int c) : 
                call_under(c){
            }
            void Snorkel::visit(Identifier& node){
                throw InternalException(std::make_shared<Identifier>(node));
            }
            void Snorkel::visit(Variable& node){
                auto p = return_value(node,call_under);
                if(p.second != nullptr){
                    //message.add_explore(node,to_reproductive(p.second));
                }
            }
            void Snorkel::visit(Number& node){
            }
            void Snorkel::visit(String& node){
            }
            void Snorkel::visit(Enum& node){
                for(sp<Expression> e : node.expressions){
                    e->accept(*this);
                }
            }
            void Snorkel::visit(Interval& node){
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
            void Snorkel::visit(Block& node){
                for(sp<Statement> s : node.statements){
                    s->accept(*this);
                }
            }
            void Snorkel::visit(Function& node){
                node.body->accept(*this);
            }
            void Snorkel::visit(FunctionCall& node){
                node.function->accept(*this);
                for(sp<Expression> a : node.arguments){
                    a->accept(*this);
                }
            }
            void Snorkel::visit(BinaryOperator& node){
                node.left->accept(*this);
                node.right->accept(*this);
            }
            void Snorkel::visit(UnaryOperator& node){
                node.exp->accept(*this);
            }
            void Snorkel::visit(IfElseExpression& node){
                node.pred->accept(*this);
                node.ife->accept(*this);
                node.elsee->accept(*this);
            }
            void Snorkel::visit(Definement& node){
                node.value->accept(*this);
            }
            void Snorkel::visit(ReturnStatement& node){
                node.value->accept(*this);
            }
            void Snorkel::visit(IfElseStatement& node){
                node.pred->accept(*this);
                node.ifblock->accept(*this);
                node.elseblock->accept(*this);
                if(return_value(node,call_under).second != nullptr){
                    auto if_p = return_value(*(node.ifblock),call_under);
                    auto else_p = return_value(*(node.elseblock),call_under);
                    if(if_p.second == nullptr and else_p.second == nullptr){
                        //error.
                    }else{
                        bool if_called = (if_p.second != nullptr
                                and else_p.second != nullptr
                                and if_p.first < else_p.first)
                            or (else_p.second == nullptr);
                        if(if_called){
                            message.add_strike(*(node.elseblock));
                        }else{
                            message.add_strike(*(node.ifblock));
                        }
                    }
                }
            }
            // should return forstatement?
            void Snorkel::visit(ForStatement& node){
                node.loop_exp->accept(*this);
                node.block->accept(*this);
            }
            void Snorkel::visit(Assignment& node){
                node.value->accept(*this);
            }
            void Snorkel::visit(TestFlyLine& node){
                throw InternalException(std::make_shared<TestFlyLine>(node));
            }
            void Snorkel::visit(IdleFlyLine& node){
                throw InternalException(std::make_shared<IdleFlyLine>(node));
            }
            void Snorkel::visit(SourceCode& node){
                for(sp<Statement> s : node.statements){
                    s->accept(*this);
                }
            }
        }
    }
}

