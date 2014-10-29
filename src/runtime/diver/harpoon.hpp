#ifndef HARPOON_HPP_INCLUDED
#define HARPOON_HPP_INCLUDED

#include "diving_message.hpp"
#include "../../syntax/ast.hpp"

namespace shiranui{
    namespace runtime{
        namespace diver{
            struct Harpoon : syntax::ast::VisitorForAST{
                DivingMessage message;
                void visit(syntax::ast::Identifier& node){
                }
                void visit(syntax::ast::Variable& node){
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
                    for(auto s : node.statements){
                        s->accept(*this);
                    }
                    for(auto s : node.flymarks){
                        s->accept(*this);
                    }
                }
                void visit(syntax::ast::Function& node){
                    node.body->accept(*this);
                }
                void visit(syntax::ast::FunctionCall& node){
                    node.function->accept(*this);
                    for(auto a : node.arguments){
                        a->accept(*this);
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
                    node.value->accept(*this);
                }
                void visit(syntax::ast::ExpressionStatement& node){
                    node.exp->accept(*this);
                }
                void visit(syntax::ast::ReturnStatement& node){
                    node.value->accept(*this);
                }
                void visit(syntax::ast::ProbeStatement& node){
                    node.value->accept(*this);
                }
                void visit(syntax::ast::AssertStatement& node){
                    node.value->accept(*this);
                }
                void visit(syntax::ast::IfElseStatement& node){
                    node.pred->accept(*this);
                    node.ifblock->accept(*this);
                    node.elseblock->accept(*this);
                }
                // should return forstatement?
                void visit(syntax::ast::ForStatement& node){
                    node.loop_exp->accept(*this);
                    node.block->accept(*this);
                }
                void visit(syntax::ast::Assignment& node){
                    node.value->accept(*this);
                }
                void visit(syntax::ast::TestFlyLine& node){
                }
                void visit(syntax::ast::IdleFlyLine& node){
                }
                void visit(syntax::ast::FlyMark& node){
                    std::string returned_value;
                    for(size_t j=0;j<node.runtime_info.visit_time.size();j++){
                        int i = node.runtime_info.visit_time[j];
                        returned_value += to_reproductive(node.runtime_info.return_value[i]);
                        if(j+1 != node.runtime_info.visit_time.size()){
                            returned_value += ",";
                        }
                    }
                    message.add_flymark_result(node,returned_value);
                }
                void visit(syntax::ast::SourceCode& node){
                    for(auto s : node.statements){
                        s->accept(*this);
                    }
                }
            };
            DivingMessage use_harpoon(syntax::ast::SourceCode& source){
                Harpoon h;
                h.visit(source);
                return h.message;
            }
        }
    }
}
#endif
