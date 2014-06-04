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
                : val(e){
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

// PrettyPrinter.
namespace shiranui{
    namespace syntax{
        namespace ast{
            std::ostream& Identifier      ::accept(VisitorForAST<std::ostream&>& visitor){
                return visitor.visit(*this);
            }
            std::ostream& Variable        ::accept(VisitorForAST<std::ostream&>& visitor){
                return visitor.visit(*this);
            }
            std::ostream& Number          ::accept(VisitorForAST<std::ostream&>& visitor){
                return visitor.visit(*this);
            }
            std::ostream& String          ::accept(VisitorForAST<std::ostream&>& visitor){
                return visitor.visit(*this);
            }
            std::ostream& Block           ::accept(VisitorForAST<std::ostream&>& visitor){
                return visitor.visit(*this);
            }
            std::ostream& Function        ::accept(VisitorForAST<std::ostream&>& visitor){
                return visitor.visit(*this);
            }
            std::ostream& FunctionCall    ::accept(VisitorForAST<std::ostream&>& visitor){
                return visitor.visit(*this);
            }
            std::ostream& BinaryOperator  ::accept(VisitorForAST<std::ostream&>& visitor){
                return visitor.visit(*this);
            }
            std::ostream& UnaryOperator   ::accept(VisitorForAST<std::ostream&>& visitor){
                return visitor.visit(*this);
            }
            std::ostream& IfElseExpression::accept(VisitorForAST<std::ostream&>& visitor){
                return visitor.visit(*this);
            }
            std::ostream& Definement      ::accept(VisitorForAST<std::ostream&>& visitor){
                return visitor.visit(*this);
            }
            std::ostream& ReturnStatement ::accept(VisitorForAST<std::ostream&>& visitor){
                return visitor.visit(*this);
            }
            std::ostream& IfElseStatement::accept(VisitorForAST<std::ostream&>& visitor){
                return visitor.visit(*this);
            }
            std::ostream& SourceCode      ::accept(VisitorForAST<std::ostream&>& visitor){
                return visitor.visit(*this);
            }
        }
    }
    std::ostream& PrettyPrinter::visit(syntax::ast::Identifier& id){
        return os << id.name;
    }
    std::ostream& PrettyPrinter::visit(syntax::ast::Variable& var){
        return os << var.value;
    }
    std::ostream& PrettyPrinter::visit(syntax::ast::Number& num){
        return os << num.value;
    }
    std::ostream& PrettyPrinter::visit(syntax::ast::String& str){
        return os << str.value;
    }
    std::ostream& PrettyPrinter::visit(syntax::ast::Block& block){
        os << "{" << std::endl;
        for(const auto& s : block.statements){
            os << *s << std::endl;
        }
        os << "}";
        return os;
    }
    std::ostream& PrettyPrinter::visit(syntax::ast::Function& func){
        os << "\\(";
        for(size_t i=0;i<func.parameters.size();i++){
            os << func.parameters[i];
            if(i != func.parameters.size()-1){
                os << ",";
            }
        }
        os << ")" << std::endl;
        os << *(func.body);
        return os ;
    }
    std::ostream& PrettyPrinter::visit(syntax::ast::FunctionCall& call){
        os << *(call.function) << "(";
        for(size_t i=0;i<call.arguments.size();i++){
            os << *(call.arguments[i]);
            if(i != call.arguments.size()-1){
                os << ",";
            }
        }
        return os << ")";

    }
    std::ostream& PrettyPrinter::visit(syntax::ast::BinaryOperator& bop){
        os << "(" << *(bop.left) << " " << bop.op << " " << *(bop.right) << ")";
        return os;
    }
    std::ostream& PrettyPrinter::visit(syntax::ast::UnaryOperator& uop){
        os << "(" << uop.op << "(" << *(uop.exp) << ")" << ")";
        return os;

    }
    std::ostream& PrettyPrinter::visit(syntax::ast::IfElseExpression& iee){
        os << "if " << *(iee.pred) << " then" << std::endl;
        os << *(iee.ife) << std::endl;
        os << "else" << std::endl;
        os << *(iee.elsee) << std::endl;
        return os;
    }
    std::ostream& PrettyPrinter::visit(syntax::ast::IfElseStatement& ies){
        os << "if " << *(ies.pred) << " then" << std::endl;
        os << *(ies.ifblock) << std::endl;
        os << "else" << std::endl;
        os << *(ies.elseblock) << std::endl;
        return os;
    }
    std::ostream& PrettyPrinter::visit(syntax::ast::Definement& def){
        return os << (def.is_const?"let ":"mut ") << def.id << "-> " << *(def.value);
    }
    std::ostream& PrettyPrinter::visit(syntax::ast::ReturnStatement& ret){
        return os << "return " << *(ret.val);
    }
    std::ostream& PrettyPrinter::visit(syntax::ast::SourceCode& sc){
        for(auto& s : sc.statements){
            os << *s << std::endl;
        }
        return os;
    }
}
namespace shiranui{
    namespace syntax{
        namespace ast{
            sp<ValEnv> Identifier      ::accept(VisitorForAST<sp<ValEnv>>& visitor){
                return visitor.visit(*this);
            }
            sp<ValEnv> Variable        ::accept(VisitorForAST<sp<ValEnv>>& visitor){
                return visitor.visit(*this);
            }
            sp<ValEnv> Number          ::accept(VisitorForAST<sp<ValEnv>>& visitor){
                return visitor.visit(*this);
            }
            sp<ValEnv> String          ::accept(VisitorForAST<sp<ValEnv>>& visitor){
                return visitor.visit(*this);
            }
            sp<ValEnv> Block           ::accept(VisitorForAST<sp<ValEnv>>& visitor){
                return visitor.visit(*this);
            }
            sp<ValEnv> Function        ::accept(VisitorForAST<sp<ValEnv>>& visitor){
                return visitor.visit(*this);
            }
            sp<ValEnv> FunctionCall    ::accept(VisitorForAST<sp<ValEnv>>& visitor){
                return visitor.visit(*this);
            }
            sp<ValEnv> BinaryOperator  ::accept(VisitorForAST<sp<ValEnv>>& visitor){
                return visitor.visit(*this);
            }
            sp<ValEnv> UnaryOperator   ::accept(VisitorForAST<sp<ValEnv>>& visitor){
                return visitor.visit(*this);
            }
            sp<ValEnv> IfElseExpression::accept(VisitorForAST<sp<ValEnv>>& visitor){
                return visitor.visit(*this);
            }
            sp<ValEnv> Definement      ::accept(VisitorForAST<sp<ValEnv>>& visitor){
                return visitor.visit(*this);
            }
            sp<ValEnv> ReturnStatement ::accept(VisitorForAST<sp<ValEnv>>& visitor){
                return visitor.visit(*this);
            }
            sp<ValEnv> IfElseStatement::accept(VisitorForAST<sp<ValEnv>>& visitor){
                return visitor.visit(*this);
            }
            sp<ValEnv> SourceCode      ::accept(VisitorForAST<sp<ValEnv>>& visitor){
                return visitor.visit(*this);
            }
        }
    }
}
