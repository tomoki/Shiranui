#include "value_printer.hpp"
#include "environment.hpp"
#include "../syntax/lambda_man.hpp"
#include <sstream>
#include <queue>

namespace shiranui{
    namespace runtime{
        namespace value{
            std::string next_name(std::string s){
                if(s == "z"){
                    return "aa";
                }
                if(s.back() == 'z'){
                    return next_name(s.substr(0,s.size()-1)) + "a";
                }
                s.back() = s.back()+1;
                return s;
            }

            // find value appearing twice
            struct ValueScanner : VisitorForValue{
                sp<ast::SourceCode> code;
                std::map<Value*,int> cnt;
                ValueScanner(sp<ast::SourceCode> code_)
                    : code(code_) {};

                void visit(Integer& node){}
                void visit(String& node){}
                void visit(Boolean& node){}
                void visit(Array& node){
                    cnt[&node]++;
                    if(cnt[&node] >= 2) return;
                    for(sp<Value> p : node.value){
                        p->accept(*this);
                    }
                }
                void visit(UserFunction& node){
                    cnt[&node]++;
                    if(cnt[&node] >= 2) return;
                    if(code != nullptr){
                        if(code->where_is_function_from.find(node.body)
                           != code->where_is_function_from.end()){
                            sp<ast::Function> f = code->where_is_function_from[node.body];
                            auto syntactic_frees = syntax::scan_free_variable(f);
                            auto free_vars = filter_environment(node.env,syntactic_frees);
                            for(auto p : free_vars){
                                p.second->accept(*this);
                            }
                        }
                    }
                }
                void visit(Return& node){cnt[&node]++;}
                void visit(SystemCall& node){cnt[&node]++;}
                void visit(BuiltinFunction& node){cnt[&node]++;}
                void visit(Ref& node){
                    cnt[&node]++;
                    if(cnt[&node] >= 2) return;
                    node.to->accept(*this);
                }
            };

            struct Task{
                virtual bool is_done() = 0;
                virtual void proceed() = 0;
                virtual std::string get_result() = 0;
                virtual Value* get_what() = 0;
                // pass sourcecode for userfunction
                virtual std::vector<sp<Task> > get_child_tasks(sp<syntax::ast::SourceCode>) = 0;
                virtual bool is_special_form() = 0;

                bool already_appeared = false;
                bool first_appeared = false;
                std::string dsl_name = "";
                void is_already_appeared(std::string s){
                    dsl_name = s;
                    already_appeared = true;
                }
                void is_first_appeared(std::string s){
                    dsl_name = s;
                    first_appeared = true;
                }
            };
            template<typename P>
            sp<Task> make_task(P);

            struct IntegerTask : Task{
                Integer* what;
                IntegerTask(Integer* w) : what(w) {}
                bool is_done(){return true;}
                std::vector<sp<Task> > get_child_tasks(sp<syntax::ast::SourceCode>){return {};}
                void proceed(){}
                bool is_special_form(){return false;}
                Value* get_what(){return what;}
                std::string get_result(){
                    std::stringstream ss;
                    ss << what->value;
                    return ss.str();
                }
            };
            struct StringTask : Task{
                String* what;
                StringTask(String* w) : what(w) {}
                bool is_done(){return true;}
                std::vector<sp<Task> > get_child_tasks(sp<syntax::ast::SourceCode>){return {};}
                void proceed(){}
                bool is_special_form(){return false;}
                Value* get_what(){return what;}
                std::string get_result(){
                    return '\"' + what->value + '\"';
                }
            };
            struct InvalidTask : Task{
                std::string message;
                InvalidTask(std::string m) : message(m) {}
                bool is_done(){return true;}
                std::vector<sp<Task> > get_child_tasks(sp<syntax::ast::SourceCode>){return {};}
                void proceed(){}
                bool is_special_form(){return false;}
                Value* get_what(){return nullptr;}
                std::string get_result(){
                    return '\"' + message + '\"';
                }
            };
            struct BooleanTask : Task{
                Boolean* what;
                BooleanTask(Boolean* w) : what(w) {}
                bool is_done(){return true;}
                std::vector<sp<Task> > get_child_tasks(sp<syntax::ast::SourceCode>){return {};}
                void proceed(){}
                bool is_special_form(){return false;}
                Value* get_what(){return what;}
                std::string get_result(){
                    std::string ret = what->value?"true":"false";
                    return ret;
                }
            };
            struct ArrayTask : Task{
                Array* what;
                int child_tasks;
                std::vector<int> in_progress_tasks;
                std::vector<sp<Task> > tasks;
                ArrayTask(Array* w) : what(w) {}
                bool is_done(){return already_appeared or child_tasks == 0;}
                bool is_special_form(){
                    if(already_appeared or first_appeared) return true;
                    for(auto t : tasks){
                        if(t->is_special_form()) return true;
                    }
                    return false;
                }
                Value* get_what(){return what;}
                std::vector<sp<Task> > get_child_tasks(sp<syntax::ast::SourceCode>){
                    child_tasks = what->value.size();
                    for(int i=0;i<child_tasks;i++){
                        in_progress_tasks.push_back(i);
                        tasks.push_back(make_task(what->value[i]));
                    }
                    return tasks;
                }
                void proceed(){
                    if(is_done()) return;
                    std::vector<int> in_progress_next;
                    for(int i : in_progress_tasks){
                        if(not tasks[i]->is_done()){
                            in_progress_next.push_back(i);
                            tasks[i]->proceed();
                        }
                    }
                    child_tasks = in_progress_next.size();
                }
                std::string get_result(){
                    if(already_appeared) return dsl_name;
                    std::string ret = "";
                    if(first_appeared){
                        ret += dsl_name + "=";
                    }
                    ret += "[";
                    for(int i=0;i<tasks.size();i++){
                        if(i != 0) ret += ",";
                        ret += tasks[i]->get_result();
                    }
                    ret += "]";
                    return ret;
                }
            };
            struct UserFunctionTask : Task{
                UserFunction* what;
                int child_tasks;
                std::vector<int> in_progress_tasks;
                std::vector<std::string> names;
                std::vector<sp<Task> > tasks;
                bool unknown = false;
                bool no_name = false;
                std::string func_id = "";
                UserFunctionTask(UserFunction* w) : what(w) {}
                bool is_done(){return already_appeared or child_tasks == 0;}
                bool is_special_form(){
                    return true; // userfunction require special form
                }
                Value* get_what(){return what;}
                std::vector<sp<Task> > get_child_tasks(sp<syntax::ast::SourceCode> code){
                    if(code == nullptr){
                        unknown = true;
                        return {};
                    }
                    if(code->where_is_function_from.find(what->body)
                               == code->where_is_function_from.end()){
                        unknown = true;
                        return {};
                    }
                    auto f = code->where_is_function_from[what->body];
                    if(f->lambda_id.name.size() == 0){
                        no_name = true;
                        return {};
                    }
                    func_id = f->lambda_id.name;
                    auto syntactic_frees = syntax::scan_free_variable(f);

                    // if global has var,they should not be included
                    std::map<syntax::ast::Identifier,sp<value::Value> > free_not_global_vars;
                    {
                        auto free_vars = filter_environment(what->env,syntactic_frees);
                        auto global_env = what->env;
                        while(global_env->parent != nullptr){
                            global_env = global_env->parent;
                        }
                        for(auto p : free_vars){
                            if(global_env->has(p.first) and (!is_ref_or_array(p.second) and
                                                             !is_userfunction(p.second))) continue;
                            free_not_global_vars[p.first] = p.second;
                        }
                    }
                    child_tasks = free_not_global_vars.size();
                    {
                        int i = 0;
                        for(auto p : free_not_global_vars){
                            in_progress_tasks.push_back(i);
                            names.push_back(p.first.name);
                            tasks.push_back(make_task(p.second));
                            i++;
                        }
                    }
                    return tasks;
                }
                void proceed(){
                    if(is_done()) return;
                    std::vector<int> in_progress_next;
                    for(int i : in_progress_tasks){
                        if(not tasks[i]->is_done()){
                            in_progress_next.push_back(i);
                            tasks[i]->proceed();
                        }
                    }
                    child_tasks = in_progress_next.size();
                }
                std::string get_result(){
                    if(already_appeared) return dsl_name;
                    if(unknown) return "$()unknown";
                    if(no_name) return "$()no_name";
                    std::string ret = "";
                    if(first_appeared){
                        ret += dsl_name + "=";
                    }
                    ret += "$(";
                    for(int i=0;i<tasks.size();i++){
                        if(i != 0) ret += ",";
                        ret += names[i] + "->" + tasks[i]->get_result();
                    }
                    ret += ")";
                    ret += func_id;
                    return ret;
                }
            };
            struct RefTask : Task{
                Ref* what;
                bool child_done = false;
                sp<Task> child;
                RefTask(Ref* w) : what(w) {}
                bool is_done(){return already_appeared or child_done;}
                bool is_special_form(){
                    if(already_appeared or first_appeared) return true;
                    if(child->is_special_form()) return true;
                    return false;
                }
                Value* get_what(){return what;}
                std::vector<sp<Task> > get_child_tasks(sp<syntax::ast::SourceCode>){
                    child = make_task(what->to);
                    return {child};
                }
                void proceed(){
                    if(is_done()) return;
                    child->proceed();
                    child_done = child->is_done();
                }
                std::string get_result(){
                    if(already_appeared) return dsl_name;
                    std::string ret = "";
                    if(first_appeared){
                        ret += dsl_name + "=";
                    }
                    ret += "ref ";
                    ret += child->get_result();
                    return ret;
                }
            };

            struct TaskMaker : VisitorForValue{
                sp<Task> ret;
                void visit(Integer& i)         {ret = std::make_shared<IntegerTask>(&i);}
                void visit(String& s)          {ret = std::make_shared<StringTask>(&s);}
                void visit(Boolean& b)         {ret = std::make_shared<BooleanTask>(&b);}
                void visit(Array& a)           {ret = std::make_shared<ArrayTask>(&a);}
                void visit(UserFunction& u)    {ret = std::make_shared<UserFunctionTask>(&u);}
                void visit(Ref& re)            {ret = std::make_shared<RefTask>(&re);}
                void visit(Return&)            {ret = std::make_shared<InvalidTask>("return?");}
                void visit(SystemCall&)        {ret = std::make_shared<InvalidTask>("SystemCall?");}
                void visit(BuiltinFunction& bf){ret = std::make_shared<InvalidTask>(bf.name);}
            };
            template<typename P>
            sp<Task> make_task(P v){
                TaskMaker t;
                v->accept(t);
                return t.ret;
            }

            void bfs_to_check_task_should_be_skipped(sp<Task> t_,sp<Value> top,
                                                     sp<syntax::ast::SourceCode> code){
                ValueScanner s(code);
                top->accept(s);
                auto cnt = s.cnt;
                std::map<Value*,std::string> name;
                std::queue<sp<Task> > que;
                que.push(t_);
                std::string current_name = "a";
                while(not que.empty()){
                    sp<Task> t = que.front();
                    que.pop();
                    Value* what = t->get_what();
                    if(what == nullptr) continue;
                    // check cnt to exclude integer,string,...
                    if(cnt.find(what) != cnt.end() and cnt[what] >= 2){
                        if(name.find(what) != name.end()){
                            t->is_already_appeared(name[what]);
                            continue;
                        }else{
                            t->is_first_appeared(current_name);
                            name[what] = current_name;
                            current_name = next_name(current_name);
                        }
                    }
                    for(auto c : t->get_child_tasks(code)){
                        que.push(c);
                    }
                }
            }
        }
    }
}

namespace shiranui{
    namespace runtime{
        namespace value{
            std::string to_reproductive(sp<Value> vi,sp<syntax::ast::SourceCode> w){
                auto task = make_task(&*vi);
                bfs_to_check_task_should_be_skipped(task,vi,w);
                while(not task->is_done()){
                    task->proceed();
                }
                if(task->is_special_form()){
                    return "<|" + task->get_result() + "|>";
                }else{
                    return task->get_result();
                }
            }
        }
    }
}
