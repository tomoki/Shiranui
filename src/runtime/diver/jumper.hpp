#ifndef JUMPER_HPP_INCLUDED
#define JUMPER_HPP_INCLUDED

#include "diving_message.hpp"
#include "../../syntax/ast.hpp"

namespace shiranui{
    namespace runtime{
        namespace diver{
            struct Jumper : syntax::ast::VisitorForAST{
                sp<syntax::ast::SourceCode> source;
                int point,index;
                // which function include flymark
                syntax::ast::Function* function;
                int call_under;
                Jumper(sp<syntax::ast::SourceCode> s,
                       const int point_,const int index_)
                    : source(s),point(point_),index(index_),function(nullptr),call_under(-1) {}
                void visit(syntax::ast::Identifier& node){
                }
                void visit(syntax::ast::Variable& node){
                }
                void visit(syntax::ast::Number& node){
                }
                void visit(syntax::ast::String& node){
                }
                void visit(syntax::ast::Boolean& node){
                }
                void visit(syntax::ast::Enum& node){
                    if(not in_range(node)) return;
                    for(auto e : node.expressions){
                        e->accept(*this);
                    }
                }
                void visit(syntax::ast::Interval& node){
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
                void visit(syntax::ast::Block& node){
                    if(not in_range(node)) return;
                    for(auto s : node.statements){
                        s->accept(*this);
                    }
                    for(auto s : node.flymarks){
                        s->accept(*this);
                    }
                }
                void visit(syntax::ast::Function& node){
                    if(not in_range(node)) return;
                    function = &node;
                    node.body->accept(*this);
                }
                void visit(syntax::ast::FunctionCall& node){
                    if(not in_range(node)) return;
                    node.function->accept(*this);
                    for(auto a : node.arguments){
                        a->accept(*this);
                    }
                }
                void visit(syntax::ast::BinaryOperator& node){
                    if(not in_range(node)) return;
                    node.left->accept(*this);
                    node.right->accept(*this);
                }
                void visit(syntax::ast::UnaryOperator& node){
                    if(not in_range(node)) return;
                    node.exp->accept(*this);
                }
                void visit(syntax::ast::IfElseExpression& node){
                    if(not in_range(node)) return;
                    node.pred->accept(*this);
                    node.ife->accept(*this);
                    node.elsee->accept(*this);
                }

                void visit(syntax::ast::Definement& node){
                    if(not in_range(node)) return;
                    node.value->accept(*this);
                }
                void visit(syntax::ast::ExpressionStatement& node){
                    if(not in_range(node)) return;
                    node.exp->accept(*this);
                }
                void visit(syntax::ast::ReturnStatement& node){
                    if(not in_range(node)) return;
                    node.value->accept(*this);
                }
                void visit(syntax::ast::ProbeStatement& node){
                    if(not in_range(node)) return;
                    node.value->accept(*this);
                }
                void visit(syntax::ast::AssertStatement& node){
                    if(not in_range(node)) return;
                    node.value->accept(*this);
                }
                void visit(syntax::ast::IfElseStatement& node){
                    if(not in_range(node)) return;
                    node.pred->accept(*this);
                    node.ifblock->accept(*this);
                    node.elseblock->accept(*this);
                }
                // should return forstatement?
                void visit(syntax::ast::ForStatement& node){
                    if(not in_range(node)) return;
                    node.loop_exp->accept(*this);
                    node.block->accept(*this);
                }
                void visit(syntax::ast::Assignment& node){
                    if(not in_range(node)) return;
                    node.value->accept(*this);
                }
                void visit(syntax::ast::TestFlyLine& node){
                }
                void visit(syntax::ast::IdleFlyLine& node){
                }
                void visit(syntax::ast::FlyMark& node){
                    if(not in_range(node)) return;
                    int time = node.runtime_info.visit_time[index];
                    for(auto p : node.runtime_info.call_under){
                        if(p.second == time){
                            call_under = p.first;
                        }
                    }
                }
                void visit(syntax::ast::SourceCode& node){
                    if(not in_range(node)) return;
                    for(auto s : node.statements){
                        s->accept(*this);
                    }
                }
                void visit(syntax::ast::DSL::DataDSL& node){
                    if(not in_range(node)) return;
                }
                template<typename T>
                bool in_range(T& t){
                    int start_point = t.point;
                    int end_point = start_point + t.length;
                    return start_point <= point and point <= end_point;
                }
                template<typename T>
                bool in_range(sp<T> t){
                    int start_point = t->point;
                    int end_point = start_point + t->length;
                    return start_point <= point and point <= end_point;
                }
            };
            std::pair<int,sp<syntax::ast::Block>> use_jumper(sp<syntax::ast::SourceCode> source,
                                     const int point,const int index){
                Jumper h(source,point,index);
                h.visit(*source);
                if(h.function != nullptr and h.call_under >= 0){
                    return std::make_pair(h.call_under,h.function->body);
                }else{
                    return std::make_pair(-1,nullptr);
                }
            }
        }
    }
}

#endif
