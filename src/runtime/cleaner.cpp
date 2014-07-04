#include "cleaner.hpp"

namespace shiranui{
    namespace runtime{
        namespace infomation{
            using namespace syntax::ast;
            void Cleaner::visit(Identifier& node){
                clear_it(node);
            }
            void Cleaner::visit(Variable& node){
                clear_it(node);
            }
            void Cleaner::visit(Number& node){
                clear_it(node);
            }
            void Cleaner::visit(String& node){
                clear_it(node);
            }
            void Cleaner::visit(Enum& node){
                clear_it(node);
                for(sp<Expression> e : node.expressions){
                    e->accept(*this);
                }
            }
            void Cleaner::visit(Interval& node){
                clear_it(node);
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
            void Cleaner::visit(Block& node){
                clear_it(node);
                for(sp<Statement> s : node.statements){
                    s->accept(*this);
                }
            }
            void Cleaner::visit(Function& node){
                clear_it(node);
                node.body->accept(*this);
            }
            void Cleaner::visit(FunctionCall& node){
                clear_it(node);
                node.function->accept(*this);
                for(sp<Expression> a : node.arguments){
                    a->accept(*this);
                }
            }
            void Cleaner::visit(BinaryOperator& node){
                clear_it(node);
                node.left->accept(*this);
                node.right->accept(*this);
            }
            void Cleaner::visit(UnaryOperator& node){
                clear_it(node);
                node.exp->accept(*this);
            }
            void Cleaner::visit(IfElseExpression& node){
                clear_it(node);
                node.pred->accept(*this);
                node.ife->accept(*this);
                node.elsee->accept(*this);
            }
            void Cleaner::visit(Definement& node){
                clear_it(node);
                node.value->accept(*this);
            }
            void Cleaner::visit(ReturnStatement& node){
                clear_it(node);
                node.value->accept(*this);
            }
            void Cleaner::visit(IfElseStatement& node){
                clear_it(node);
                node.pred->accept(*this);
                node.ifblock->accept(*this);
                node.elseblock->accept(*this);
            }
            // should return forstatement?
            void Cleaner::visit(ForStatement& node){
                clear_it(node);
                node.loop_exp->accept(*this);
                node.block->accept(*this);
            }
            void Cleaner::visit(Assignment& node){
                clear_it(node);
                node.value->accept(*this);
            }
            void Cleaner::visit(TestFlyLine& node){
                clear_it(node);
                if(node.left != nullptr){
                    node.left->accept(*this);
                }
                if(node.right != nullptr){
                    node.right->accept(*this);
                }
            }
            void Cleaner::visit(IdleFlyLine& node){
                clear_it(node);
                if(node.left != nullptr){
                    node.left->accept(*this);
                }
                if(node.right != nullptr){
                    node.right->accept(*this);
                }
            }
            void Cleaner::visit(SourceCode& node){
                clear_it(node);
                for(sp<Statement> s : node.statements){
                    s->accept(*this);
                }
            }
            template<typename T>
            void Cleaner::clear_it(T& t){
                t.runtime_info.clear();
            }
        }
    }
}
