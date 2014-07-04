#include "tester.hpp"
#include "../server/server.hpp"
#include <iostream>

namespace shiranui{
    namespace tester{
        using namespace server;
        std::string make_change(int point,int remove_length,std::string s){
            std::stringstream ss;
            ss << point << " " << remove_length << " " << how_many_lines(s) << std::endl
               << s;
            return ss.str();
        }
        void wait(int milli){
            boost::this_thread::sleep(boost::posix_time::milliseconds(milli));
        }
        void run_test(){
            std::stringstream in,out;
            PipeServer ps(in,std::cerr);
            // donot start.
            ps.receive_command(COMMAND_CHANGE,make_change(1,0,"let f = \\(n){return n;};"));
            wait(100);
        }
    }
}

