#include "server.hpp"
#include <sstream>
#include <chrono>

namespace shiranui{
    namespace server{
        const std::string COMMAND_CHANGE = "change";
        const std::string COMMAND_DEBUG_PRINT = "debug";
        const std::string COMMAND_SYNTAXEROR = "syntaxerror";
        const std::string COMMAND_IDLE_FLYLINE = "idleflyline";
        const std::string COMMAND_GOOD_FLYLINE = "goodflyline";
        const std::string COMMAND_BAD_FLYLINE = "badflyline";

        std::string to_reproductive(sp<runtime::value::Value>);
        int calc_point(const std::string&,int,int);
        int how_many_lines(const std::string&);

        PipeServer::PipeServer(std::istream& i,std::ostream& o)
            : is(i),os(o){
        }

        void PipeServer::start(){
            boost::thread receive(&PipeServer::receive,this);
            receive.join();
        }

        void PipeServer::send_command(const std::string& command,const std::string& value){
            std::stringstream ss;
            boost::this_thread::interruption_point();
            os_lock.lock();
            if(value != ""){
                os << how_many_lines(value) << " " << command << std::endl
                   << value << std::endl;
            }else{
                os << how_many_lines(value) << " " << command << std::endl;
            }
            os_lock.unlock();
        }
        void PipeServer::send_command_with_two_points(const std::string& command,
                                                      const int& start_point,
                                                      const int& end_point){
            std::stringstream ss;
            ss << start_point << " " << end_point;
            return send_command(command,ss.str());
        }
        void PipeServer::send_syntaxerror(const int& start_point,const int& end_point){
            return send_command_with_two_points(COMMAND_SYNTAXEROR,start_point,end_point);
        }
        void PipeServer::send_good_flyline(const int& start_point,const int& end_point){
            return send_command_with_two_points(COMMAND_GOOD_FLYLINE,start_point,end_point);
        }
        void PipeServer::send_bad_flyline(const int& start_point,const int& end_point){
            return send_command_with_two_points(COMMAND_BAD_FLYLINE,start_point,end_point);
        }
        void PipeServer::send_idle_flyline(const int& start_point,const int& end_point,
                                           const int& remove_start,const int& remove_end,
                                           const int& insert_point,const std::string& value){
            std::stringstream ss;
            ss << start_point << " " << end_point << std::endl
               << remove_start << " " << remove_end << std::endl
               << insert_point << std::endl
               << value;
            return send_command(COMMAND_IDLE_FLYLINE,ss.str());
        }

        void PipeServer::send_debug_print(const std::string& value){
            return send_command(COMMAND_DEBUG_PRINT,value);
        }

        // receive
        void PipeServer::receive(){
            while(true){
                int line;
                std::string command;
                while(is >> line >> command){
                    is.ignore(); // ignore newline.
                    std::string value;
                    for(int i=0;i<line;i++){
                        std::string s;
                        // getline doesn't have newline.
                        std::getline(is,s);
                        value += s;
                        if(i != line-1){
                            value += "\n";
                        }
                    }
                    receive_command(command,value);
                }
                is.clear(); // remove eof flag 
            }
        }
        void PipeServer::receive_command(const std::string& command,
                                         const std::string& value){
            if(command == COMMAND_CHANGE){
                return on_change_command(value);
            }
        }

        void PipeServer::on_change_command(const std::string& value){
            main_thread.interrupt();
            flyline_threads.interrupt_all();
            std::stringstream ss(value);
            while(not ss.eof()){
                int point,remove_length,line_size;
                std::string insert_value;
                ss >> point >> remove_length >> line_size;
                point--;
                ss.ignore(); // remove newline.
                for(int i=0;i<line_size;i++){
                    std::string s;
                    getline(ss,s);
                    insert_value += s;
                    if(i != line_size - 1){
                        insert_value += '\n';
                    }
                }
                source.erase(point,remove_length);
                source.insert(point,insert_value);
            }
            main_thread.join();
            flyline_threads.join_all();
            main_thread = boost::thread(boost::bind(&PipeServer::exec,this,source));
        }

        void PipeServer::exec(std::string source){
            using namespace shiranui;
            using namespace shiranui::syntax;
            using namespace shiranui::syntax::ast;
            using namespace shiranui::runtime;
            const auto start_time = std::chrono::system_clock::now();

            pos_iterator_t first(source.begin()),last(source.end());
            pos_iterator_t iter = first;
            bool ok = false;
            Parser<pos_iterator_t> resolver(first);
            sp<SourceCode> program;
            try{
                ok = boost::spirit::qi::phrase_parse(iter,last,resolver,
                                                     boost::spirit::qi::space,program);
            }catch (boost::spirit::qi::expectation_failure<pos_iterator_t> const& x){
                send_syntaxerror(std::distance(first,x.first),std::distance(first,x.last));
                return;
            }

            if(ok and iter == last){
                for(int i=0;i<program->flylines.size();i++){
                    flyline_threads.create_thread(boost::bind(&PipeServer::run_flyline,
                                this,source,i));
                }
            }else{
                send_syntaxerror(std::distance(first,iter),std::distance(first,last));
            }
            const auto end_time = std::chrono::system_clock::now();
            const auto time_span = end_time - start_time;
            std::stringstream ts;
            ts << "First path:" << std::chrono::duration_cast<std::chrono::milliseconds>(time_span).count() << "[ms]";
            send_command(COMMAND_DEBUG_PRINT,ts.str());

        }

        void PipeServer::run_flyline(std::string source,int flyline_index){
            using namespace shiranui;
            using namespace shiranui::syntax;
            using namespace shiranui::syntax::ast;
            using namespace shiranui::runtime;

            pos_iterator_t first(source.begin()),last(source.end());
            pos_iterator_t iter = first;
            bool ok = false;
            Parser<pos_iterator_t> resolver(first);
            sp<SourceCode> program;
            ok = boost::spirit::qi::phrase_parse(iter,last,resolver,
                                                 boost::spirit::qi::space,program);


            Runner r;
            sp<FlyLine> sf = program->flylines[flyline_index];
            try{
                program->accept(r);
            }catch(NoSuchVariableException e){
                return;
            }catch(ConvertException e){
                return;
            }catch(RuntimeException e){
                return;
            }

            {
                sp<TestFlyLine> l = std::dynamic_pointer_cast<TestFlyLine>(sf);
                if(l != nullptr) run_testflyline(source,r,l);
            }
            {
                sp<IdleFlyLine> l = std::dynamic_pointer_cast<IdleFlyLine>(sf);
                if(l != nullptr) run_idleflyline(source,r,l);
            }
        }

        void PipeServer::run_testflyline(std::string source,
                                         runtime::Runner r,
                                         sp<syntax::ast::TestFlyLine> sf){
            using namespace syntax::ast;
            using namespace runtime::value;
            using namespace shiranui::runtime;

            int start_point = calc_point(source,sf->line,sf->column);
            int end_point = start_point + sf->length;
            if(sf->right != nullptr){
                auto bin = std::make_shared<BinaryOperator>("=",sf->left,sf->right);
                try{
                    bin->accept(r);
                }catch(NoSuchVariableException e){
                    send_syntaxerror(start_point,end_point);
                    return;
                }catch(ConvertException e){
                    send_syntaxerror(start_point,end_point);
                    return;
                }catch(RuntimeException e){
                    send_syntaxerror(start_point,end_point);
                    return;
                }

                sp<Boolean> b = std::dynamic_pointer_cast<Boolean>(r.cur.v);
                if(b != nullptr){
                    if(b->value){
                        send_good_flyline(start_point,end_point);
                    }else{
                        send_bad_flyline(start_point,end_point);
                    }
                }else{
                    send_syntaxerror(start_point,end_point);
                }
            }else{ // right == null
                send_syntaxerror(start_point,end_point);
            }
        }

        void PipeServer::run_idleflyline(std::string source,
                                         runtime::Runner r,
                                         sp<syntax::ast::IdleFlyLine> sf){
            using namespace syntax::ast;
            using namespace runtime::value;
            using namespace shiranui::runtime;
            int start_point = calc_point(source,sf->line,sf->column);
            int end_point = start_point + sf->length;
            try{
                sf->left->accept(r);
            }catch(NoSuchVariableException e){
                send_syntaxerror(start_point,end_point);
                return;
            }catch(ConvertException e){
                send_syntaxerror(start_point,end_point);
                return;
            }catch(RuntimeException e){
                send_syntaxerror(start_point,end_point);
                return;
            }

            sp<Value> left = r.cur.v;
            std::string left_str = to_reproductive(left);
            if(sf->right != nullptr){
                int remove_start = calc_point(source,sf->right->line,sf->right->column);
                int remove_end = remove_start + sf->right->length;
                send_idle_flyline(start_point,end_point,remove_start,remove_end
                                 ,remove_start,left_str);
            }else{ // right == null
                // reproduce
                // do not delete anything
                send_idle_flyline(start_point,end_point,1,1,end_point-1,left_str);
                // send_change_flyline_to_idle();
            }
        }



        // helper functions.
        std::string to_reproductive(sp<runtime::value::Value> vi){
            using namespace runtime::value;
            {
                sp<Integer> v = std::dynamic_pointer_cast<Integer>(vi);
                if(v != nullptr){
                    std::stringstream ss;
                    ss << v->value;
                    return ss.str();
                }
            }
            {
                sp<String> v = std::dynamic_pointer_cast<String>(vi);
                if(v != nullptr){
                    std::stringstream ss;
                    ss << '"' << v->value << '"';
                    return ss.str();
                }
            }
            {
                sp<Array> v = std::dynamic_pointer_cast<Array>(vi);
                if(v != nullptr){
                    std::stringstream ss;
                    ss << "[";
                    for(int i=0;i<v->value.size();i++){
                        ss << to_reproductive(v->value[i]);
                        if(i != v->value.size()-1){
                            ss << ",";
                        }
                    }
                    ss << "]";
                    return ss.str();
                }
            }
            return "unknown";
        }
        int calc_point(const std::string& source,int line,int column){
            int li=1,co=1;
            int point = 1;
            for(const char& c : source){
                if(line == li and column == co){
                    return point;
                }
                if(c == '\n'){
                    co = 1;li++;
                }else{
                    co++;
                }
                point++;
            }
            return -1;
        }
        int how_many_lines(const std::string& s){
            if(s == "") return 0;
            int cnt = 1;
            for(char c : s){
                if(c == '\n') cnt++;
            }
            return cnt;
        }
    }
}
