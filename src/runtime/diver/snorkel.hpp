#ifndef SNORKEL_HPP_INCLUDED
#define SNORKEL_HPP_INCLUDED

#include "diving_message.hpp"
#include "../value_printer.hpp"
#include "../runtime_info.hpp"
#include "../../syntax/ast.hpp"
#include "../runner.hpp"

namespace shiranui{
    namespace runtime{
        namespace diver{

            struct Snorkel : syntax::ast::VisitorForAST{
                DivingMessage message;
                std::vector<sp<syntax::ast::FunctionCall>> lift_candidate;
                sp<syntax::ast::SourceCode> source;
                int call_under;
                // Snorkel
                Snorkel(sp<syntax::ast::SourceCode> s,int c) :
                    source(s),call_under(c){
                }
                bool is_used_statement(syntax::ast::Statement& s){
                    return infomation::return_value(s,call_under).second != nullptr;
                }
                void visit(syntax::ast::Identifier& node){
                    throw InternalException(std::make_shared<syntax::ast::Identifier>(node));
                }
                void visit(syntax::ast::Variable& node){
                    auto p = infomation::return_value(node,call_under);
                    if(p.second != nullptr){
                        message.add_explore(node,to_reproductive(p.second,source));
                    }
                }
                void visit(syntax::ast::Number& node){
                }
                void visit(syntax::ast::String& node){
                }
                void visit(syntax::ast::Boolean& node){
                }
                void visit(syntax::ast::Enum& node){
                    for(auto e : node.expressions){
                        e->accept(*this);
                    }
                }
                void visit(syntax::ast::Interval& node){
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
                    if(not is_used_statement(node)){
                        message.add_strike(node);
                        return;
                    }
                    message.add_highlight(node);
                    for(auto s : node.statements){
                        s->accept(*this);
                    }
                    for(auto s : node.flymarks){
                        s->accept(*this);
                    }
                }
                void visit(syntax::ast::Function& node){
                    // node.body->accept(*this);
                    return;
                }
                void visit(syntax::ast::FunctionCall& node){
                    node.function->accept(*this);
                    for(auto a : node.arguments){
                        a->accept(*this);
                    }
                    auto p = infomation::return_value(node,call_under);
                    if(p.second != nullptr){
                        message.add_explore(node,to_reproductive(p.second,source));
                        // TODO: should not copy
                        lift_candidate.push_back(std::make_shared<syntax::ast::FunctionCall>(node));
                    }
                }
                void visit(syntax::ast::BinaryOperator& node){
                    node.left->accept(*this);
                    node.right->accept(*this);
                }
                void visit(syntax::ast::UnaryOperator& node){
                    node.exp->accept(*this);
                }
                void visit(syntax::ast::IfElseExpression& node){
                    node.pred->accept(*this);
                    node.ife->accept(*this);
                    node.elsee->accept(*this);
                }

                void visit(syntax::ast::Definement& node){
                    if(not is_used_statement(node)){
                        message.add_strike(node);
                        return;
                    }
                    node.value->accept(*this);
                }
                void visit(syntax::ast::ExpressionStatement& node){
                    if(not is_used_statement(node)){
                        message.add_strike(node);
                        return;
                    }
                    node.exp->accept(*this);
                }
                void visit(syntax::ast::ReturnStatement& node){
                    if(not is_used_statement(node)){
                        message.add_strike(node);
                        return;
                    }
                    node.value->accept(*this);
                }
                void visit(syntax::ast::ProbeStatement& node){
                    if(not is_used_statement(node)){
                        message.add_strike(node);
                        return;
                    }
                    node.value->accept(*this);
                }
                void visit(syntax::ast::AssertStatement& node){
                    if(not is_used_statement(node)){
                        message.add_strike(node);
                        return;
                    }
                    node.value->accept(*this);
                }
                void visit(syntax::ast::IfElseStatement& node){
                    if(not is_used_statement(node)){
                        message.add_strike(node);
                        return;
                    }
                    node.pred->accept(*this);
                    node.ifblock->accept(*this);
                    // if there is no elseblock,we create one.
                    //  it has point 0 and length 0.
                    if(node.elseblock->point > 0 and
                       node.elseblock->length > 0){
                        node.elseblock->accept(*this);
                    }
                }
                // should return forstatement?
                void visit(syntax::ast::ForStatement& node){
                    if(not is_used_statement(node)){
                        message.add_strike(node);
                        return;
                    }
                    node.loop_exp->accept(*this);
                    node.block->accept(*this);
                }
                void visit(syntax::ast::Assignment& node){
                    if(not is_used_statement(node)){
                        message.add_strike(node);
                        return;
                    }
                    node.value->accept(*this);
                }
                void visit(syntax::ast::TestFlyLine& node){
                    throw InternalException(std::make_shared<syntax::ast::TestFlyLine>(node));
                }
                void visit(syntax::ast::IdleFlyLine& node){
                    throw InternalException(std::make_shared<syntax::ast::IdleFlyLine>(node));
                }
                void visit(syntax::ast::FlyMark& node){
                    int when = node.runtime_info.index_of_called(call_under);
                    if(when < 0) return;
                    message.add_flymark_index(node,when);
                }
                void visit(syntax::ast::SourceCode& node){
                    for(auto s : node.statements){
                        s->accept(*this);
                    }
                }
                void visit(syntax::ast::DSL::DataDSL& node){
                }

            };
            template<typename T>
            DivingMessage use_snorkel(sp<syntax::ast::SourceCode> sc,T& block,int call_under){
                Snorkel s(sc,call_under);
                s.visit(block);
                return s.message;
            }
            template<typename T>
            std::vector<sp<syntax::ast::FunctionCall> >  get_lift_candidate(
                                          sp<syntax::ast::SourceCode> sc,T& block,int call_under){
                Snorkel s(sc,call_under);
                s.visit(block);
                return s.lift_candidate;
            }
        }
    }
}
#endif
