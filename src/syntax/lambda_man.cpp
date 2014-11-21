#include "lambda_man.hpp"

namespace shiranui{
    namespace syntax{
        void LambdaMarkerScanner::visit(ast::Identifier&){}
        void LambdaMarkerScanner::visit(ast::Variable&){}
        void LambdaMarkerScanner::visit(ast::Number&){}
        void LambdaMarkerScanner::visit(ast::String&){}
        void LambdaMarkerScanner::visit(ast::Enum& node){
            for(auto e : node.expressions){
                e->accept(*this);
            }
        }
        void LambdaMarkerScanner::visit(ast::Interval& node){
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
        void LambdaMarkerScanner::visit(ast::Block& node){
            for(auto s : node.statements){
                s->accept(*this);
            }
            for(auto s : node.flymarks){
                s->accept(*this);
            }
        }
        void LambdaMarkerScanner::visit(ast::Function& node){
            // FIXME: copy is safe,BUT should not.(runtime_infomation is not shared)
            auto copy = std::make_shared<ast::Function>(node);
            where_are_you_from[node.body] = copy;
            if(node.lambda_id.name != ""){
                marker_to_lambda[node.lambda_id] = copy;
            }
            node.body->accept(*this);
        }
        void LambdaMarkerScanner::visit(ast::FunctionCall& node){
            node.function->accept(*this);
            for(auto a : node.arguments){
                a->accept(*this);
            }
        }
        void LambdaMarkerScanner::visit(ast::BinaryOperator& node){
            node.left->accept(*this);
            node.right->accept(*this);
        }
        void LambdaMarkerScanner::visit(ast::UnaryOperator& node){
            node.exp->accept(*this);
        }
        void LambdaMarkerScanner::visit(ast::IfElseExpression& node){
            node.pred->accept(*this);
            node.ife->accept(*this);
            node.elsee->accept(*this);
        }

        void LambdaMarkerScanner::visit(ast::Definement& node){
            node.value->accept(*this);
        }
        void LambdaMarkerScanner::visit(ast::ExpressionStatement& node){
            node.exp->accept(*this);
        }
        void LambdaMarkerScanner::visit(ast::ReturnStatement& node){
            node.value->accept(*this);
        }
        void LambdaMarkerScanner::visit(ast::ProbeStatement& node){
            node.value->accept(*this);
        }
        void LambdaMarkerScanner::visit(ast::AssertStatement& node){
            node.value->accept(*this);
        }
        void LambdaMarkerScanner::visit(ast::IfElseStatement& node){
            node.pred->accept(*this);
            node.ifblock->accept(*this);
            node.elseblock->accept(*this);
        }
        // should return forstatement?
        void LambdaMarkerScanner::visit(ast::ForStatement& node){
            node.loop_exp->accept(*this);
            node.block->accept(*this);
        }
        void LambdaMarkerScanner::visit(ast::Assignment& node){
            node.value->accept(*this);
        }
        void LambdaMarkerScanner::visit(ast::TestFlyLine&){}
        void LambdaMarkerScanner::visit(ast::IdleFlyLine&){}
        void LambdaMarkerScanner::visit(ast::FlyMark&){}
        void LambdaMarkerScanner::visit(ast::SourceCode& node){
            for(auto s : node.statements){
                s->accept(*this);
            }
        }
        void LambdaMarkerScanner::visit(ast::DSL::DataDSL&){}
        std::pair<
            std::map<sp<ast::Block>,sp<ast::Function> >,
            std::map<ast::Identifier,sp<ast::Function> >
            >
        scan_lambda_marker(ast::SourceCode& source){
            LambdaMarkerScanner h;
            h.visit(source);
            return std::make_pair(h.where_are_you_from,
                                  h.marker_to_lambda);
        }
    }
}
