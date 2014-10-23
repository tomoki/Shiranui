#ifndef SNORKEL_HPP_INCLUDED
#define SNORKEL_HPP_INCLUDED

#include "diving_message.hpp"
#include "diver.hpp"
#include "../../syntax/ast.hpp"
#include "../runner.hpp"

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
            struct Snorkel : syntax::ast::VisitorForAST{

                int call_under;
                DivingMessage message;
                // Snorkel
                Snorkel(int c) : 
                    call_under(c){
                }
                bool is_used_statement(syntax::ast::Statement& s){
                    return return_value(s,call_under).second != nullptr;
                }
                void visit(syntax::ast::Identifier& node){
                    throw InternalException(std::make_shared<syntax::ast::Identifier>(node));
                }
                void visit(syntax::ast::Variable& node){
                    auto p = return_value(node,call_under);
                    if(p.second != nullptr){
                        message.add_explore(node,to_reproductive(p.second));
                    }
                }
                void visit(syntax::ast::Number& node){
                }
                void visit(syntax::ast::String& node){
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
                    for(auto s : node.statements){
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
                    auto p = return_value(node,call_under);
                    if(p.second != nullptr){
                        message.add_explore(node,to_reproductive(p.second));
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
                    node.elseblock->accept(*this);
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
                    throw InternalException(std::make_shared<syntax::ast::FlyMark>(node));
                }
                void visit(syntax::ast::SourceCode& node){
                    for(auto s : node.statements){
                        s->accept(*this);
                    }
                }
            };
            template<typename T>
            DivingMessage use_snorkel(T& block,int call_under){
                Snorkel s(call_under);
                s.visit(block);
                return s.message;
            }
        }
    }
}
#endif
