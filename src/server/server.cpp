#include "server.hpp"

namespace shiranui{
    namespace server{
        const std::string COMMAND_LOAD = "load";
        const std::string COMMAND_SYNTAXEROR = "syntaxerror";
        int how_many_lines(const std::string& s){
            if(s == "") return 0;
            int cnt = 1;
            for(char c : s){
                if(c == '\n') cnt++;
            }
            return cnt;
        }
        PipeServer::PipeServer(std::istream& is_,std::ostream& os_)
            : is(is_),os(os_){}

        void PipeServer::start(){
            std::thread receive_thread(&PipeServer::receive_command,this);
            receive_thread.join();
        }

        void PipeServer::send_command(const std::string& command,const std::string& value){
            send_lock.lock();
            if(value != ""){
                os << how_many_lines(value) << " " << command << std::endl
                   << value << std::endl;
            }else{
                os << how_many_lines(value) << " " << command << std::endl;
            }
            send_lock.unlock();
        }
        // call from thread.
        void PipeServer::receive_command(){
            while(true){
                int line;
                std::string command;
                while(is >> line >> command){
                    is.ignore(); // ignore newline.
                    std::string value;
                    for(int i=0;i<line;i++){
                        std::string s;
                        std::getline(is,s);
                        value += s;
                    }
                    on_receive_command(command,value);
                }
                is.clear(); // remove eof flag 
            }
        }
        void PipeServer::on_receive_command(const std::string& command,
                                            const std::string& value){
            if(command == COMMAND_LOAD){
                on_receive_load_command(value);
            }
        }
        // copy string because pos_iterator_t needs its reference.
        void PipeServer::on_receive_load_command(std::string source){
            using namespace shiranui;
            using namespace shiranui::syntax;
            using namespace shiranui::runtime;
            Runner r;
            ast::PrettyPrinterForAST printer(std::cerr);
            value::PrettyPrinterForValue printer_for_value(std::cerr);

            pos_iterator_t first(source.begin()),last(source.end());
            pos_iterator_t iter = first;
            bool ok = false;
            Parser<pos_iterator_t> resolver(first);
            try{
                ok = boost::spirit::qi::phrase_parse(iter,last,resolver,
                        boost::spirit::qi::space,program);
            }catch (boost::spirit::qi::expectation_failure<pos_iterator_t> const& x){
                send_command(COMMAND_SYNTAXEROR,"");
                return;
            }

            if(ok and iter == last){
                // there is no syntax error.
                // first path,shiranui doesn't eval flytestline.
                try{
                    program->accept(r);
                    //r.cur.v->accept(printer_for_value);
                    //std::cerr << std::endl;
                }catch(NoSuchVariableException e){
                    //std::cerr << "No such variable: ";
                    //e.where->accept(printer);
                    //std::cerr << std::endl;
                }catch(ConvertException e){
                    //std::cerr << "Convert Error: ";
                    //e.where->accept(printer);
                    //std::cerr << std::endl;
                }catch(RuntimeException e){
                    //std::cerr << "Something RuntimeException: ";
                    //e.where->accept(printer);
                    //std::cerr << std::endl;
                }
            }else{
                send_command(COMMAND_SYNTAXEROR,"");
            }
        }
    }
}
