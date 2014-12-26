#include "cleaner.hpp"
#include "environment.hpp"

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
            void Cleaner::visit(Boolean& node){
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
            void Cleaner::visit(ExpressionStatement& node){
                clear_it(node);
                node.exp->accept(*this);
            }
            void Cleaner::visit(ReturnStatement& node){
                clear_it(node);
                node.value->accept(*this);
            }
            void Cleaner::visit(ProbeStatement& node){
                clear_it(node);
                node.value->accept(*this);
            }
            void Cleaner::visit(AssertStatement& node){
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
            void Cleaner::visit(FlyMark& node){
                clear_it(node);
                if(node.left != nullptr){
                    node.left->accept(*this);
                }
            }
            void Cleaner::visit(SourceCode& node){
                clear_it(node);
                for(sp<Statement> s : node.statements){
                    s->accept(*this);
                }
            }
            void Cleaner::visit(DSL::DataDSL& dsl){
            }
            template<typename T>
            void Cleaner::clear_it(T& t){
                ValueCleaner c;
                for(auto p : t.runtime_info.return_value){
                    if(p.second.first != nullptr){
                        p.second.first->accept(c);
                    }
                }
                t.runtime_info.clear();
            }

            using namespace runtime::value;
            void ValueCleaner::visit(Integer&){
            }
            void ValueCleaner::visit(String&){
            }
            void ValueCleaner::visit(Boolean&){
            }
            void ValueCleaner::visit(Array& v){
                if(already.find(&v) != already.end()) return;
                already.insert(&v);
                for(sp<Value> p : v.value){
                    p->accept(*this);
                }
                v.value.clear();
            }
            void ValueCleaner::visit(UserFunction& v){
                if(already.find(&v) != already.end()) return;
                already.insert(&v);
                v.env->clear();
            }
            void ValueCleaner::visit(Return& v){
                if(already.find(&v) != already.end()) return;
                already.insert(&v);
                v.value->accept(*this);
            }
            void ValueCleaner::visit(SystemCall&){
            }
            void ValueCleaner::visit(BuiltinFunction&){
            }
        }
    }
}

