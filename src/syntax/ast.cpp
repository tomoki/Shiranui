#include "ast.hpp"

namespace shiranui{
    namespace syntax{
        namespace ast{
            // Identifier
            Identifier::Identifier(std::string n) : name(n) {};
            Identifier::Identifier(std::vector<char> n) : name(n.begin(),n.end()) {}
            bool Identifier::operator<(const Identifier& id) const{
                return name < id.name;
            }

            // Variable
            Variable::Variable(Identifier v) : value(v) {}

            // Number
            Number::Number(int v):value(v) {}

            // String
            String::String(std::string v):value(v) {}
            String::String(std::vector<char> v):value(v.begin(),v.end()) {}

            Enum::Enum(std::vector<sp<Expression>> es) : expressions(es) {};
            Enum::Enum() {};

            Interval::Interval(sp<Expression> s,sp<Expression> e)
                                                : start(s),end(e) {}
            Interval::Interval(sp<Expression> s,sp<Expression> n,sp<Expression> e)
                                                : start(s),end(e),next(n) {}

            // Block
            Block::Block(std::vector<sp<Statement>> ss)
                : statements(ss){
            }
            // Function
            Function::Function(std::vector<Identifier> params,sp<Block> ss)
                : parameters(params),body(ss){
            }

            // FunctionCall
            FunctionCall::FunctionCall(sp<Expression> i,std::vector<sp<Expression>> as)
                : function(i),arguments(as){
            }

            // BinaryOperator
            BinaryOperator::BinaryOperator(std::string o,sp<Expression> l,sp<Expression> r)
                : op(o),left(l),right(r){
            }
            // UnaryOperator
            UnaryOperator::UnaryOperator(std::string o,sp<Expression> e)
                : op(o),exp(e){
            }

            // IfElseExpression
            IfElseExpression::IfElseExpression(sp<Expression> p,sp<Expression> ib,sp<Expression> eb)
                : pred(p),ife(ib),elsee(eb){
            }

            // Definement
            Definement::Definement(Identifier i,sp<Expression> e,bool isc)
                : id(i),value(e),is_const(isc) {}

            // IfElseStatement
            IfElseStatement::IfElseStatement(sp<Expression> e,sp<Block> iblock)
                : pred(e),ifblock(iblock),elseblock(new Block({})){
            }
            IfElseStatement::IfElseStatement(sp<Expression> e,sp<Block> iblock,sp<Block> eblock)
                : pred(e),ifblock(iblock),elseblock(eblock){
            }

            // ReturnStatement
            ReturnStatement::ReturnStatement(sp<Expression> e)
                : value(e){
            }

            Assignment::Assignment(Identifier i,sp<Expression> e)
                : id(i),value(e){}
            // FlyLine
            FlyLine::FlyLine(sp<Expression> l)
                : left(l){
            }
            FlyLine::FlyLine(sp<Expression> l,sp<Expression> r)
                : left(l),right(r){
            }

            // SourceCode
            SourceCode::SourceCode(){
            }
            SourceCode::SourceCode(std::vector<sp<Statement>> ss)
                : statements(ss){
            }
            void SourceCode::add_statement(sp<Statement> s){
                statements.push_back(s);
            }
            void SourceCode::add_flyline(sp<FlyLine> l){
                flylines.push_back(l);
            }
            // LocationInfo
        }
    }
}

namespace shiranui{
    namespace syntax{
        namespace ast{
            void Identifier      ::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }
            void Variable        ::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }
            void Number          ::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }
            void String          ::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }
            void Enum            ::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }
            void Interval        ::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }

            void Block           ::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }
            void Function        ::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }
            void FunctionCall    ::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }
            void BinaryOperator  ::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }
            void UnaryOperator   ::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }
            void IfElseExpression::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }
            void Definement      ::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }
            void ReturnStatement ::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }
            void IfElseStatement ::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }
            void Assignment       ::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }
            void FlyLine          ::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }
            void SourceCode      ::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }
        }
    }
}

// PrettyPrinterForAST.
namespace shiranui{
    namespace syntax{
        namespace ast{
            void PrettyPrinterForAST::visit(syntax::ast::Identifier& id){
                os << id.name;
            }
            void PrettyPrinterForAST::visit(syntax::ast::Variable& var){
                var.value.accept(*this);
            }
            void PrettyPrinterForAST::visit(syntax::ast::Number& num){
                os << num.value;
            }
            void PrettyPrinterForAST::visit(syntax::ast::String& str){
                os << str.value;
            }
            void PrettyPrinterForAST::visit(syntax::ast::Enum& enu){
                os << "[";
                for(sp<Expression> exp : enu.expressions){
                    exp->accept(*this);
                }
                os << "]";
            }

            void PrettyPrinterForAST::visit(syntax::ast::Interval& intr){
                os << "[";
                intr.start->accept(*this);
                os << ",";
                if(intr.next != nullptr){
                    intr.next->accept(*this);
                }else{
                    os << "?";
                }
                os << "..";
                intr.end->accept(*this);
                os << "]";
            }
            void PrettyPrinterForAST::visit(syntax::ast::Block& block){
                os << ind() << "{" << std::endl;
                indent += 1;
                for(auto& s : block.statements){
                    s->accept(*this);
                    os << std::endl;
                }
                indent -= 1;
                os << ind() << "}";
            }
            void PrettyPrinterForAST::visit(syntax::ast::Function& func){
                os << "\\(";
                for(size_t i=0;i<func.parameters.size();i++){
                    func.parameters[i].accept(*this);
                    if(i != func.parameters.size()-1){
                        os << ",";
                    }
                }
                os << ")" << std::endl;
                func.body->accept(*this);
            }
            void PrettyPrinterForAST::visit(syntax::ast::FunctionCall& call){
                call.function->accept(*this);
                os << "(";
                for(size_t i=0;i<call.arguments.size();i++){
                    call.arguments[i]->accept(*this);
                    if(i != call.arguments.size()-1){
                        os << ",";
                    }
                }
                os << ")";
            }
            void PrettyPrinterForAST::visit(syntax::ast::BinaryOperator& bop){
                os << "(";
                bop.left->accept(*this);
                os << " " << bop.op << " " ;
                bop.right->accept(*this);
                os << ")";
            }
            void PrettyPrinterForAST::visit(syntax::ast::UnaryOperator& uop){
                os << "(" << uop.op;
                uop.exp->accept(*this);
                os << ")";
            }
            void PrettyPrinterForAST::visit(syntax::ast::IfElseExpression& iee){
                os << ind() << "if ";
                iee.pred->accept(*this);
                os << " then" << std::endl;
                iee.ife->accept(*this);
                os << std::endl;
                os << "else" << std::endl;
                iee.elsee->accept(*this);
                os << std::endl;
            }
            void PrettyPrinterForAST::visit(syntax::ast::IfElseStatement& ies){
                os << ind() << "if ";
                ies.pred->accept(*this);
                os << " then " << std::endl;
                ies.ifblock->accept(*this);
                os << " else " << std::endl;
                ies.elseblock->accept(*this);
            }
            void PrettyPrinterForAST::visit(syntax::ast::Definement& def){
                os << ind() << (def.is_const?"let ":"mut ");
                def.id.accept(*this);
                os << " = ";
                def.value->accept(*this);
                os << ";";
            }
            void PrettyPrinterForAST::visit(syntax::ast::ReturnStatement& ret){
                os << ind() << "return ";
                ret.value->accept(*this);
                os << ";";
            }
            void PrettyPrinterForAST::visit(syntax::ast::Assignment& l){
                os << ind();
                l.id.accept(*this);
                os << " <- ";
                l.value->accept(*this);
                os << ";";
            }

            void PrettyPrinterForAST::visit(syntax::ast::FlyLine& l){
                os << ind() << "#- ";
                l.left->accept(*this);
            }
            void PrettyPrinterForAST::visit(syntax::ast::SourceCode& sc){
                for(auto& s : sc.statements){
                    s->accept(*this);
                    os << std::endl;
                }
                for(auto& s : sc.flylines){
                    s->accept(*this);
                    os << std::endl;
                }
            }
            std::string PrettyPrinterForAST::ind(){
                return std::string(indent*4,' ');
            }
        }
    }
}
