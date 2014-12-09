#include "ast.hpp"
#include "../runtime/cleaner.hpp"

namespace shiranui{
    namespace syntax{
        namespace ast{
            // Identifier
            Identifier::Identifier(std::string n) : name(n) {};
            Identifier::Identifier(std::vector<char> n) : name(n.begin(),n.end()) {}
            bool Identifier::operator<(const Identifier& id) const{
                return name < id.name;
            }
            bool Identifier::operator==(const Identifier& id) const{
                return name == id.name;
            }


            // Variable
            Variable::Variable(Identifier v) : value(v) {}

            // Number
            Number::Number(int v):value(v) {}

            // String
            String::String(std::string v):value(v) {}
            String::String(std::vector<char> v):value(v.begin(),v.end()) {}

            Boolean::Boolean(bool v):value(v) {}

            Enum::Enum(std::vector<sp<Expression>> es) : expressions(es) {};
            Enum::Enum() {};

            Interval::Interval(sp<Expression> s,sp<Expression> n,sp<Expression> e,bool r)
                                : start(s),end(e),next(n),right_close(r) {}

            // Block
            Block::Block(){
            }
            void Block::add_statement(sp<Statement> s){
                statements.push_back(s);
            }
            void Block::add_pre(sp<Block> f){
                pre.push_back(f);
            }
            void Block::add_post(Identifier i,sp<Block> f){
                post_id.push_back(i);
                post.push_back(f);
            }
            void Block::add_invariant(sp<Block> i){
                invariant.push_back(i);
            }
            void Block::add_flymark(sp<FlyMark> mark){
                flymarks.push_back(mark);
            }
            // Function
            Function::Function(std::vector<Identifier> params,sp<Block> ss)
                : parameters(params),body(ss){
            }
            Function::Function(Identifier id,std::vector<Identifier> params,sp<Block> ss)
                : lambda_id(id),parameters(params),body(ss){
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

            ExpressionStatement::ExpressionStatement(sp<Expression> e)
                : exp(e){
            }
            // IfElseStatement
            IfElseStatement::IfElseStatement(sp<Expression> e,sp<Block> iblock)
                : pred(e),ifblock(iblock),elseblock(new Block({})){
            }
            IfElseStatement::IfElseStatement(sp<Expression> e,sp<Block> iblock,sp<Block> eblock)
                : pred(e),ifblock(iblock),elseblock(eblock){
            }
            ForStatement::ForStatement(Identifier i,sp<Expression> e,sp<Block> b)
                : loop_var(i),loop_exp(e),block(b) {}

            // ReturnStatement
            ReturnStatement::ReturnStatement(sp<Expression> e)
                : value(e){
            }
            ProbeStatement::ProbeStatement(sp<Expression> e)
                : value(e){
            }
            AssertStatement::AssertStatement(sp<Expression> e)
                : value(e){
            }
            Assignment::Assignment(Identifier i,sp<Expression> e)
                : id(i),value(e){}
            // FlyLine
            TestFlyLine::TestFlyLine(sp<Expression> l,sp<Expression> r,sp<Expression> e)
                : left(l),right(r),error(e){
            }
            IdleFlyLine::IdleFlyLine(sp<Expression> l,sp<Expression> r)
                : left(l),right(r){
            }

            FlyMark::FlyMark(sp<Expression> l)
                : left(l){
            }
            FlyMark::FlyMark(sp<Expression> l,std::vector<sp<Expression>> r)
                : left(l),right(r){
            }
            // SourceCode
            SourceCode::SourceCode(){
            }
            SourceCode::~SourceCode(){
                runtime::infomation::Cleaner c;
                accept(c);
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
            namespace DSL{
                DSLVariable::DSLVariable() : name("") {}
                DSLVariable::DSLVariable(std::string n) : name(n) {}
                DSLVariable::DSLVariable(std::vector<char> n) : name(n.begin(),n.end()) {}
                bool DSLVariable::operator<(const DSLVariable& id) const{
                    return name < id.name;
                }
                DSLDefine::DSLDefine(sp<DSLVariable> f,sp<DSLInner> t) : var(f),value(t) {}
                DSLInteger::DSLInteger(int v) : value(v) {}
                DSLBoolean::DSLBoolean(bool v) : value(v) {}
                DSLString::DSLString(std::string v) : value(v) {}
                DSLArray::DSLArray() {};
                DSLArray::DSLArray(std::vector<sp<DSLInner>> v) : value(v){}
                DSLFunction::DSLFunction(myenv e,sp<DSLVariable> id): environment(e),lambda_id(id->name) {}
                DataDSL::DataDSL(sp<DSLInner> v) : inner(v) {}
            }
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
            void Boolean         ::accept(VisitorForAST& visitor){
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
            void ExpressionStatement::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }
            void ReturnStatement ::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }
            void ProbeStatement  ::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }
            void AssertStatement ::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }
            void IfElseStatement ::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }
            void ForStatement    ::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }
            void Assignment      ::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }
            void TestFlyLine     ::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }
            void IdleFlyLine     ::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }
            void FlyMark         ::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }
            void SourceCode      ::accept(VisitorForAST& visitor){
                return visitor.visit(*this);
            }
            namespace DSL{
                void DataDSL :: accept(VisitorForAST& visitor){
                    return visitor.visit(*this);
                }
                void DSLVariable::accept(VisitorForDSL& visitor){return visitor(*this);}
                void DSLDefine  ::accept(VisitorForDSL& visitor){return visitor(*this);}
                void DSLInteger ::accept(VisitorForDSL& visitor){return visitor(*this);}
                void DSLBoolean ::accept(VisitorForDSL& visitor){return visitor(*this);}
                void DSLString  ::accept(VisitorForDSL& visitor){return visitor(*this);}
                void DSLArray   ::accept(VisitorForDSL& visitor){return visitor(*this);}
                void DSLFunction::accept(VisitorForDSL& visitor){return visitor(*this);}
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
            void PrettyPrinterForAST::visit(syntax::ast::Boolean& b){
                os << (b.value ? "true":"false");
            }
            void PrettyPrinterForAST::visit(syntax::ast::Enum& enu){
                os << "[";
                for(size_t i=0;i<enu.expressions.size();i++){
                    enu.expressions[i]->accept(*this);
                    if(i != enu.expressions.size()){
                        os << ",";
                    }
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
                os << std::endl;
                ies.ifblock->accept(*this);
                os << " else " << std::endl;
                ies.elseblock->accept(*this);
            }
            void PrettyPrinterForAST::visit(syntax::ast::ForStatement& fo){
                os << ind() << "for ";
                fo.loop_var.accept(*this);
                os << " in ";
                fo.loop_exp->accept(*this);
                os << " ";
                fo.block->accept(*this);
            }

            void PrettyPrinterForAST::visit(syntax::ast::Definement& def){
                os << ind() << (def.is_const?"let ":"mut ");
                def.id.accept(*this);
                os << " = ";
                def.value->accept(*this);
                os << ";";
            }
            void PrettyPrinterForAST::visit(syntax::ast::ExpressionStatement& es){
                os << ind();
                es.exp->accept(*this);
                os << ";";
            }
            void PrettyPrinterForAST::visit(syntax::ast::ReturnStatement& ret){
                os << ind() << "return ";
                ret.value->accept(*this);
                os << ";";
            }
            void PrettyPrinterForAST::visit(syntax::ast::ProbeStatement& ret){
                os << ind() << "probe ";
                ret.value->accept(*this);
                os << ";";
            }
            void PrettyPrinterForAST::visit(syntax::ast::AssertStatement& as){
                os << ind() << "assert ";
                as.value->accept(*this);
                os << ";";
            }
            void PrettyPrinterForAST::visit(syntax::ast::Assignment& l){
                os << ind();
                l.id.accept(*this);
                os << " <- ";
                l.value->accept(*this);
                os << ";";
            }

            void PrettyPrinterForAST::visit(syntax::ast::TestFlyLine& l){
                os << ind() << "#- ";
                l.left->accept(*this);
                os << " -> ";
                if(l.right != nullptr){
                    l.right->accept(*this);
                }
            }
            void PrettyPrinterForAST::visit(syntax::ast::IdleFlyLine& l){
                os << ind() << "#+ ";
                l.left->accept(*this);
                os << " -> ";
                if(l.right != nullptr){
                    l.right->accept(*this);
                }
            }
            void PrettyPrinterForAST::visit(syntax::ast::FlyMark& m){
                os << ind() << "#* ";
                m.left->accept(*this);
                os << " -> ;";
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
            void PrettyPrinterForAST::visit(syntax::ast::DSL::DataDSL& dsl){
                os << "?";
            }
            std::string PrettyPrinterForAST::ind(){
                return std::string(indent*4,' ');
            }
        }
    }
}
