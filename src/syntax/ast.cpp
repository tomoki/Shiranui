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

            // Block
            Block::Block(std::vector<Statement*> ss){
                for(auto s : ss){
                    statements.push_back(sp<Statement>(s));
                }
            }
            // Function
            Function::Function(std::vector<Identifier> params,Block* ss)
                : parameters(params),body(ss){
            }

            // FunctionCall
            FunctionCall::FunctionCall(Expression* i,std::vector<Expression*> as){
                function = sp<Expression>(i);
                for(Expression* e : as){
                    arguments.push_back(sp<Expression>(e));
                }
            }

            // BinaryOperator
            BinaryOperator::BinaryOperator(std::string o,Expression* l,Expression* r){
                left = sp<Expression>(l);
                right = sp<Expression>(r);
                op = o;
            }
            // UnaryOperator
            UnaryOperator::UnaryOperator(std::string o,Expression* e){
                op = o;
                exp = sp<Expression>(e);
            }

            // IfElseExpression
            IfElseExpression::IfElseExpression(Expression* p,Expression* ib,Expression* eb){
                pred = sp<Expression>(p);
                ife  = sp<Expression>(ib);
                elsee= sp<Expression>(eb);
            }

            // Definement
            Definement::Definement(Identifier i,Expression *e,bool isc)
                : id(i),value(e),is_const(isc) {}

            // IfElseStatement
            IfElseStatement::IfElseStatement(Expression* e,Block* iblock)
                : pred(e),ifblock(iblock),elseblock(new Block({})){
            }
            IfElseStatement::IfElseStatement(Expression* e,Block* iblock,Block* eblock)
                : pred(e),ifblock(iblock),elseblock(eblock){
            }

            // ReturnStatement
            ReturnStatement::ReturnStatement(Expression* e)
                : value(e){
            }

            // SourceCode
            SourceCode::SourceCode(std::vector<Statement*> ss){
                for(Statement* s : ss){
                    statements.push_back(sp<Statement>(s));
                }
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
            void PrettyPrinterForAST::visit(syntax::ast::Block& block){
                os << "{" << std::endl;
                for(auto& s : block.statements){
                    s->accept(*this);
                    os << std::endl;
                }
                os << "}";
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
                os << "if ";
                iee.pred->accept(*this);
                os << " then" << std::endl;
                iee.ife->accept(*this);
                os << std::endl;
                os << "else" << std::endl;
                iee.elsee->accept(*this);
                os << std::endl;
            }
            void PrettyPrinterForAST::visit(syntax::ast::IfElseStatement& ies){
                os << "if ";
                ies.pred->accept(*this);
                os << " then ";
                ies.ifblock->accept(*this);
                os << " else " << std::endl;
                ies.elseblock->accept(*this);
            }
            void PrettyPrinterForAST::visit(syntax::ast::Definement& def){
                os << (def.is_const?"let ":"mut ");
                def.id.accept(*this);
                os << " -> ";
                def.value->accept(*this);
            }
            void PrettyPrinterForAST::visit(syntax::ast::ReturnStatement& ret){
                os << "return ";
                ret.value->accept(*this);
            }
            void PrettyPrinterForAST::visit(syntax::ast::SourceCode& sc){
                for(auto& s : sc.statements){
                    s->accept(*this);
                    os << std::endl;
                }
            }

        }
    }
}
