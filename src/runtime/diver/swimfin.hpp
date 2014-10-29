#ifndef SWIMFIN_HPP_INCLUDED
#define SWIMFIN_HPP_INCLUDED

#include "diving_message.hpp"
#include "../../syntax/ast.hpp"
#include "../runner.hpp"

namespace shiranui{
    namespace runtime{
        namespace diver{
            struct SwimFin : syntax::ast::VisitorForAST{
                sp<syntax::ast::Expression> treasure;
                int point;
                SwimFin(int point_)
                    : treasure(nullptr),point(point_){
                }
                void visit(syntax::ast::Identifier& node){
                    throw InternalException(std::make_shared<syntax::ast::Identifier>(node));
                }
                void visit(syntax::ast::Variable& node){
                    if(not in_range(node)) return;
                }
                void visit(syntax::ast::Number& node){
                    if(not in_range(node)) return;
                }
                void visit(syntax::ast::String& node){
                    if(not in_range(node)) return;
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
                    node.body->accept(*this);
                }
                void visit(syntax::ast::FunctionCall& node){
                    if(not in_range(node)) return;
                    treasure = std::make_shared<syntax::ast::FunctionCall>(node);
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
                    if(not in_range(node)) return;
                    throw InternalException(std::make_shared<syntax::ast::TestFlyLine>(node));
                }
                void visit(syntax::ast::IdleFlyLine& node){
                    if(not in_range(node)) return;
                    throw InternalException(std::make_shared<syntax::ast::IdleFlyLine>(node));
                }
                void visit(syntax::ast::FlyMark& node){
                    if(not in_range(node)) return;
                    // throw InternalException(std::make_shared<FlyMark>(node));
                }
                void visit(syntax::ast::SourceCode& node){
                    if(not in_range(node)) return;
                    for(auto s : node.statements){
                        s->accept(*this);
                    }
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
            template<typename T>
            sp<syntax::ast::LocationInfo> use_swimfin(T& sc,int point){
                SwimFin sf(point);
                sf.visit(sc);
                return sf.treasure; // can be null
            }
        }
    }
}

#endif
