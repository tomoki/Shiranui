#include "server.hpp"
#include "../runtime/cleaner.hpp"
#include <sstream>
#include <chrono>

namespace shiranui{
    namespace server{
        PipeServer::PipeServer(std::istream& i,std::ostream& o)
            : is(i),os(o){
        }

        void PipeServer::start(){
            boost::thread receive(&PipeServer::receive,this);
            receive.join();
        }

        void PipeServer::send_command(const std::string& command,const std::string& value,
                                      const int loadcount){
            std::stringstream ss;
            boost::this_thread::interruption_point();
            os_lock.lock();
            if(value != ""){
                os << how_many_lines(value) << " " << loadcount << " " << command << "\n"
                   << value << std::endl;
            }else{
                os << how_many_lines(value) << " " << loadcount << " " << command << std::endl;
            }
            os_lock.unlock();
        }
        void PipeServer::send_command_with_two_points(const std::string& command,
                                                      const int start_point,
                                                      const int end_point,
                                                      const int loadcount){
            std::stringstream ss;
            ss << start_point << " " << end_point;
            return send_command(command,ss.str(),loadcount);
        }
        void PipeServer::send_syntaxerror(const int start_point,const int end_point,
                                          const int loadcount){
            return send_command_with_two_points(COMMAND_SYNTAXEROR,
                                                start_point,end_point,loadcount);
        }
        void PipeServer::send_good_flyline(const int start_point,const int end_point,
                                           const int loadcount){
            return send_command_with_two_points(COMMAND_GOOD_FLYLINE,
                                                start_point,end_point,loadcount);
        }
        void PipeServer::send_bad_flyline(const int start_point,const int end_point,
                                         const int loadcount,const std::string& error){
            std::stringstream ss;
            ss << start_point << " " << end_point << std::endl;
            ss << '"' << error << '"';
            return send_command(COMMAND_BAD_FLYLINE,ss.str(),loadcount);
        }
        void PipeServer::send_runtimeerror(const int start_point,const int end_point,
                                           const int loadcount){
            return send_command_with_two_points(COMMAND_RUNTIMEERROR,
                                                start_point,end_point,loadcount);
        }


        void PipeServer::send_idle_flyline(const int start_point,const int end_point,
                                           const int insert_point,const int remove_length,
                                           const std::string& value,const int loadcount){
            std::stringstream ss;
            ss << start_point << " " << end_point << std::endl
               << insert_point << " " << remove_length << std::endl
               << value;
            return send_command(COMMAND_IDLE_FLYLINE,ss.str(),loadcount);
        }

        template<typename T>
        void PipeServer::send_debug_print(const T& value,const int loadcount){
            std::stringstream ss;
            ss << value;
            return send_command(COMMAND_DEBUG_PRINT,ss.str(),loadcount);
        }

        void PipeServer::send_dive_strike(const int start_point,const int end_point,
                                          const int loadcount){
            return send_command_with_two_points(COMMAND_DIVE_STRIKE,
                                                start_point,end_point,loadcount);
        }
        void PipeServer::send_dive_explore(const int start_point,const int end_point,
                                           const std::string& what,const int loadcount){
            std::stringstream ss;
            ss << start_point << " " << end_point << std::endl
               << what;
            return send_command(COMMAND_DIVE_EXPLORE,ss.str(),loadcount);
        }

        void PipeServer::send_dive_clear(const int loadcount){
            return send_command(COMMAND_DIVE_CLEAR,"",loadcount);
        }

        // receive
        void PipeServer::receive(){
            while(true){
                int line,loadcount;
                std::string command;
                while(is >> line >> loadcount >> command){
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
                    receive_command(command,value,loadcount);
                }
                is.clear(); // remove eof flag
            }
        }
        void PipeServer::receive_command(const std::string& command,
                                         const std::string& value,
                                         const int loadcount){
            // loadcount can be ignored by dive or surface
            if(command == COMMAND_CHANGE){
                return on_change_command(value,loadcount);
            }else if(command == COMMAND_DIVE){
                return on_dive_command(value,loadcount);
            }else if(command == COMMAND_SURFACE){
                return on_surface_command(value,loadcount);
            }
        }

        void PipeServer::on_change_command(const std::string& value,
                                           const int loadcount){
            main_thread.interrupt();
            main_thread.join();
            for(sp<boost::thread> t : flyline_threads){
                t->interrupt();
                t->join();
            }
            flyline_threads.clear();
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
            main_thread = boost::thread(boost::bind(&PipeServer::exec,this,source,loadcount));
        }

        void PipeServer::on_dive_command(const std::string& value,const int loadcount){
            std::stringstream ss(value);
            int point;ss >> point;
            // TODO: check main_thread,flyline_threads condition.
            for(int i=0;i<static_cast<int>(program_per_flyline.size());i++){
                int start_point = program_per_flyline[i]->flylines[i]->point;
                int end_point = start_point + program_per_flyline[i]->flylines[i]->length;
                if(start_point <= point and point <= end_point){
                    // currently running
                    if(diver_per_flyline[i] == nullptr){
                        std::stringstream out;
                        out << "dive_start -> " << i << std::endl
                            << " but currently running.";
                        //send_debug_print(out.str(),loadcount);
                    }else{
                        std::stringstream out;
                        out << "dive_start -> " << i;
                        //send_debug_print(out.str(),loadcount);
                        dive_start(diver_per_flyline[i],
                                program_per_flyline[i]->flylines[i]);

                    }
                    return;
                }
            }
            // maybe dive to inner function
            if(current_diver != nullptr){
                dive(current_diver,point,loadcount);
            }else{
                std::stringstream out;
                out << "cannot find where to dive." << std::endl;
                send_debug_print(out.str(),loadcount);
            }

            //boost::unique_lock<boost::mutex> lock(main_thread_end_mutex);
            //main_thread_waiting.wait(lock);
        }
        // TODO: should treat point.(if left is list...)
        void PipeServer::dive_start(sp<runtime::diver::Diver> diver,sp<syntax::ast::FlyLine> sf){
            using namespace shiranui::runtime::diver;
            using namespace shiranui::syntax::ast;
            current_diver = diver;
            diver->clear();
            {
                sp<TestFlyLine> l = std::dynamic_pointer_cast<TestFlyLine>(sf);
                if(l != nullptr){
                    DivingMessage ms = diver->dive(l->left);
                    //send_debug_print(ms.str(),-1);
                    send_diving_message(source,ms);
                }
            }
            {
                sp<IdleFlyLine> l = std::dynamic_pointer_cast<IdleFlyLine>(sf);
                if(l != nullptr){
                    DivingMessage ms = diver->dive(l->left);
                    //send_debug_print(ms.str(),-1);
                    send_diving_message(source,ms);
                }
            }
        }

        void PipeServer::dive(sp<runtime::diver::Diver> diver,int point,const int loadcount){
            using namespace shiranui::runtime::diver;
            DivingMessage ms = diver->dive(point);
            send_diving_message(source,ms);
        }
        void PipeServer::on_surface_command(const std::string&,const int loadcount){
            // value has nothing.
            if(current_diver != nullptr){
                surface(current_diver);
            }else{
            }
        }
        // undo for diver.
        void PipeServer::surface(sp<runtime::diver::Diver> diver){
            using namespace shiranui::runtime::diver;
            DivingMessage ms = diver->surface();
            send_diving_message(source,ms);
        }

        void PipeServer::exec(std::string source,const int loadcount){
            using namespace shiranui;
            using namespace shiranui::syntax;
            using namespace shiranui::syntax::ast;
            using namespace shiranui::runtime;
            using namespace shiranui::runtime::diver;
            const auto start_time = std::chrono::system_clock::now();

            pos_iterator_t first(source.begin()),last(source.end());
            pos_iterator_t iter = first;
            bool ok = false;
            Parser<pos_iterator_t> resolver;
            sp<SourceCode> program;
            try{
                // ok = boost::spirit::qi::phrase_parse(iter,last,resolver,
                //                                      boost::spirit::qi::space,program);
                ok = parse(iter,last,resolver,program);
            }catch (boost::spirit::qi::expectation_failure<pos_iterator_t> const& x){
                send_syntaxerror(std::distance(first,x.first),
                                 std::distance(first,x.last),loadcount);
                return;
            }

            if(ok and iter == last){
                Runner r(true);
                try{
                    program->accept(r);
                }catch(NoSuchVariableException e){
                    int start_point = e.where->point;
                    int end_point = start_point + e.where->length;
                    send_runtimeerror(start_point,end_point,loadcount);
                    return;
                }catch(ConvertException e){
                    int start_point = e.where->point;
                    int end_point = start_point + e.where->length;
                    send_runtimeerror(start_point,end_point,loadcount);
                    return;
                }catch(RuntimeException e){
                    int start_point = e.where->point;
                    int end_point = start_point + e.where->length;
                    send_runtimeerror(start_point,end_point,loadcount);
                    return;
                }
                // TODO: when runtimeerror occured,clean it too.
                {
                    runtime::infomation::Cleaner c;
                    program->accept(c);
                }

                for(sp<SourceCode> s : program_per_flyline){
                    runtime::infomation::Cleaner c;
                    if(s != nullptr){
                        s->accept(c);
                    }
                }
                program_per_flyline = std::vector<sp<SourceCode>>(program->flylines.size(),nullptr);
                diver_per_flyline = std::vector<sp<Diver>>(program->flylines.size(),nullptr);
                // TODO:should kill diver process
                current_diver = nullptr;

                for(int i=0;i<static_cast<int>(program->flylines.size());i++){
                    flyline_threads.push_back(std::make_shared<boost::thread>(
                             boost::bind(&PipeServer::run_flyline,this,source,i,loadcount)));
                }
            }else{
                send_syntaxerror(std::distance(first,iter),std::distance(first,last),loadcount);
            }
            const auto end_time = std::chrono::system_clock::now();
            const auto time_span = end_time - start_time;
            std::stringstream ts;
            ts << "First:" << std::chrono::duration_cast<std::chrono::milliseconds>(time_span).count() << "[ms]";
            send_debug_print(ts.str(),loadcount);
        }

        void PipeServer::run_flyline(std::string source,const int flyline_index,
                                     const int loadcount){
            using namespace shiranui;
            using namespace shiranui::syntax;
            using namespace shiranui::syntax::ast;
            using namespace shiranui::runtime;
            using namespace shiranui::runtime::diver;

            const auto start_time = std::chrono::system_clock::now();
            pos_iterator_t first(source.begin()),last(source.end());
            pos_iterator_t iter = first;
            Parser<pos_iterator_t> resolver;
            sp<SourceCode> program;
            parse(iter,last,resolver,program);

            boost::this_thread::interruption_point();
            program_per_flyline[flyline_index] = program; // TODO:use better way.
            Runner r(true);
            sp<FlyLine> sf = program->flylines[flyline_index];
            program->accept(r); // do not cause exception.

            {
                sp<TestFlyLine> l = std::dynamic_pointer_cast<TestFlyLine>(sf);
                if(l != nullptr) run_testflyline(r,l,loadcount);
            }
            {
                sp<IdleFlyLine> l = std::dynamic_pointer_cast<IdleFlyLine>(sf);
                if(l != nullptr) run_idleflyline(r,l,loadcount);
            }
            diver_per_flyline[flyline_index] = std::make_shared<Diver>(program);
            const auto end_time = std::chrono::system_clock::now();
            const auto time_span = end_time - start_time;
            std::stringstream ts;
            ts << "Exec [" << flyline_index << "]: " << std::chrono::duration_cast<std::chrono::milliseconds>(time_span).count() << "[ms]";
            //send_debug_print(ts.str(),loadcount);
        }

        void PipeServer::run_testflyline(runtime::Runner& r,
                                         sp<syntax::ast::TestFlyLine> sf,
                                         const int loadcount){
            using namespace syntax::ast;
            using namespace runtime::value;
            using namespace shiranui::runtime;

            int start_point = sf->point;
            int end_point = start_point + sf->length;
            auto bin = std::make_shared<BinaryOperator>("=",sf->left,sf->right);
            int remove_start = sf->right->point;
            int remove_length = sf->error != nullptr ?
                sf->error->point + sf->error->length - remove_start
                : 0;
            try{
                bin->accept(r);
            }catch(NoSuchVariableException e){
                send_bad_flyline(start_point,end_point,loadcount,"");
                return;
            }catch(ConvertException e){
                send_bad_flyline(start_point,end_point,loadcount,"");
                return;
            }catch(AssertException e){
                send_bad_flyline(start_point,end_point,loadcount,"");
                return;
            }catch(ZeroDivException e){
                send_bad_flyline(start_point,end_point,loadcount,"");
                return;
            }catch(RuntimeException e){
                send_bad_flyline(start_point,end_point,loadcount,"");
                return;
            }

            sp<Boolean> b = std::dynamic_pointer_cast<Boolean>(r.cur_v);
            if(b != nullptr){
                if(b->value){
                    send_good_flyline(start_point,end_point,loadcount);
                }else{
                    send_bad_flyline(start_point,end_point,loadcount,"");
                }
            }else{
                send_syntaxerror(start_point,end_point,loadcount);
            }
        }

        void PipeServer::run_idleflyline(runtime::Runner& r,
                                         sp<syntax::ast::IdleFlyLine> sf,
                                         const int loadcount){
            using namespace syntax::ast;
            using namespace runtime::value;
            using namespace shiranui::runtime;
            // TODO:save infomation.
            auto run_idleflyline_sub = [this,sf,loadcount](std::string left_str){
                int start_point = sf->point;
                int end_point = start_point + sf->length;
                if(sf->right != nullptr){
                    int remove_start = sf->right->point;
                    int remove_end = remove_start + sf->right->length;
                    int remove_length = remove_end - remove_start;
                    send_idle_flyline(start_point,end_point,remove_start,remove_length,
                                      left_str,loadcount);
                }else{
                    send_idle_flyline(start_point,end_point,end_point-1,0,left_str,loadcount);
                }
            };
            try{
                sf->left->accept(r);
            }catch(NoSuchVariableException e){
                auto p = std::dynamic_pointer_cast<Variable>(e.where);
                std::stringstream ss;
                ss << '\"' << "No such variable: " << p->value.name << '\"';
                return run_idleflyline_sub(ss.str());
            }catch(ConvertException e){
                return run_idleflyline_sub("\"Can't convert something\"");
            }catch(AssertException e){
                return run_idleflyline_sub("\"Assert violated\"");
            }catch(ZeroDivException e){
                return run_idleflyline_sub("\"Division by 0\"");
            }catch(RuntimeException e){
                return run_idleflyline_sub("\"Something occured.\"");
            }

            sp<Value> left = r.cur_v;
            std::string left_str = to_reproductive(left);
            run_idleflyline_sub(left_str);
        }

        void PipeServer::send_diving_message(const std::string&,
                                             runtime::diver::DivingMessage message){
            using namespace runtime::diver;
            std::stringstream ss(message.str());
            send_dive_clear(-1);
            std::string command;
            while(ss >> command){
                if(command == STRIKE){
                    int start_point,length;
                    ss >> start_point >> length;
                    int end_point = start_point + length;
                    send_dive_strike(start_point,end_point,-1);
                }else if(command == EXPLORE){
                    int start_point,length;
                    ss >> start_point >> length;
                    int end_point = start_point + length;
                    std::string value;
                    ss.ignore();
                    std::getline(ss,value);
                    send_dive_explore(start_point,end_point,value,-1);
                }else if(command == ERROR){
                    int start_point,length;
                    std::string what;
                    ss >> start_point >> length;
                    ss.ignore();
                    std::getline(ss,what);

                    std::stringstream out;
                    out << "ERROR: " << what << " (" << start_point 
                                     << "," << start_point + length << ")";
                    send_debug_print(out.str(),-1);
                }
            }
        }

        int how_many_lines(const std::string& s){
            if(s == "") return 0;
            return count(s.begin(),s.end(),'\n')+1;
        }
    }
}
