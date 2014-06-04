#include "ast.hpp"

namespace shiranui{
    namespace syntax{
        namespace ast{
            // Identifier
            Identifier::Identifier(std::string n) : name(n) {};
            Identifier::Identifier(std::vector<char> n) : name(n.begin(),n.end()) {}
            std::ostream& Identifier::serialize(std::ostream &os) const{
               return os << name;
            }
            bool Identifier::operator<(const Identifier& id) const{
                return name < id.name;
            }

            // Variable
            Variable::Variable(Identifier v) : value(v) {}
            std::ostream& Variable::serialize(std::ostream &os) const{
                return os << value;
            }

            // Number
            Number::Number(int v):value(v) {}
            std::ostream& Number::serialize(std::ostream &os) const{
                return os << value;
            }

            // String
            String::String(std::string v):value(v) {}
            String::String(std::vector<char> v):value(v.begin(),v.end()) {}
            std::ostream& String::serialize(std::ostream &os) const{
                return os << value;
            }

            // Block
            Block::Block(std::vector<Statement*> ss){
                for(auto s : ss){
                    statements.push_back(sp<Statement>(s));
                }
            }
            std::ostream& Block::serialize(std::ostream &os) const{
                os << "{" << std::endl;
                for(const auto& s : statements){
                    os << *s << std::endl;
                }
                os << "}";
                return os;
            }

            // Function
            Function::Function(std::vector<Identifier> params,Block* ss)
                : parameters(params),body(ss){
            }
            std::ostream& Function::serialize(std::ostream &os) const{
                os << "\\(";
                for(size_t i=0;i<parameters.size();i++){
                    os << parameters[i];
                    if(i != parameters.size()-1){
                        os << ",";
                    }
                }
                os << ")" << std::endl;
                os << *body;
                return os ;
            }

            // FunctionCall
            FunctionCall::FunctionCall(Expression* i,std::vector<Expression*> as){
                function = sp<Expression>(i);
                for(Expression* e : as){
                    arguments.push_back(sp<Expression>(e));
                }
            }
            std::ostream& FunctionCall::serialize(std::ostream &os) const{
                os << *function << "(";
                for(size_t i=0;i<arguments.size();i++){
                    os << *(arguments[i]);
                    if(i != arguments.size()-1){
                        os << ",";
                    }
                }
                return os << ")";
            }

            // BinaryOperator
            BinaryOperator::BinaryOperator(std::string o,Expression* l,Expression* r){
                left = sp<Expression>(l);
                right = sp<Expression>(r);
                op = o;
            }
            std::ostream& BinaryOperator::serialize(std::ostream &os) const{
                os << "(" << *left << " " << op << " " << *right << ")";
                return os;
            }

            // UnaryOperator
            UnaryOperator::UnaryOperator(std::string o,Expression* e){
                op = o;
                exp = sp<Expression>(e);
            }
            std::ostream& UnaryOperator::serialize(std::ostream &os) const{
                os << "(" <<  op << "(" << *exp << ")" << ")";
                return os;
            }

            // IfElseExpression
            IfElseExpression::IfElseExpression(Expression* p,Expression* ib,Expression* eb){
                pred = sp<Expression>(p);
                ife  = sp<Expression>(ib);
                elsee= sp<Expression>(eb);
            }

            std::ostream& IfElseExpression::serialize(std::ostream &os) const{
                os << "(if " << *pred << " then" << std::endl;
                os << *ife << " else  " << *elsee << ")" << std::endl;
                return os;
            }

            // Definement
            Definement::Definement(Identifier i,Expression *e,bool isc)
                : id(i),value(e),is_const(isc) {}
            std::ostream& Definement::serialize(std::ostream &os) const{
                return os << (is_const?"let ":"mut ") << id << "-> " << *value;
            }

            // IfElseStatement
            IfElseStatement::IfElseStatement(Expression* e,Block* iblock)
                : pred(e),ifblock(iblock),elseblock(new Block({})){
            }
            IfElseStatement::IfElseStatement(Expression* e,Block* iblock,Block* eblock)
                : pred(e),ifblock(iblock),elseblock(eblock){
            }
            std::ostream& IfElseStatement::serialize(std::ostream &os) const{
                os << "if " << *pred << " then" << std::endl;
                os << *ifblock << std::endl;
                os << "else" << std::endl;
                os << *elseblock << std::endl;
                return os;
            }

            // ReturnStatement
            ReturnStatement::ReturnStatement(Expression* e)
                : val(e){
            }
            std::ostream& ReturnStatement::serialize(std::ostream &os) const{
                os << "return " << *val;
                return os;
            }

            // SourceCode
            SourceCode::SourceCode(std::vector<Statement*> ss){
                for(Statement* s : ss){
                    statements.push_back(sp<Statement>(s));
                }
            }
            std::ostream& SourceCode::serialize(std::ostream &os) const{
                for(const auto& s : statements){
                    os << *s << std::endl;
                }
                return os;
            }


            // LocationInfo
            std::ostream& operator<<(std::ostream& os,
                                     const shiranui::syntax::ast::LocationInfo& s){
                return s.serialize(os);
            }
        }
    }
}
