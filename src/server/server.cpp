#include "server.hpp"

namespace shiranui{
    namespace server{
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
        }
        void PipeServer::send_command(const std::string& command,const std::string& value){
            send_lock.lock();
            os << how_many_lines(value) << " " << command << std::endl
                << value << std::endl;
            send_lock.unlock();
        }
        // call from thread.
        void PipeServer::receive_command(){
            while(true){
                int line;
                std::string command,value;
                is >> line >> command;
                for(int i=0;i<line;i++){
                    std::string s;
                    std::getline(is,s);
                    value += s;
                }
                on_receive_command(command,value);
            }
        }
        void PipeServer::on_receive_command(const std::string& command,
                                            const std::string& value){
        }
    }
}
