#include "tester.hpp"
#include "../server/server.hpp"
#include <iostream>

namespace shiranui{
    namespace tester{
        using namespace server;
        using namespace std;
        string make_change(int point,int remove_length,string s){
            stringstream ss;
            ss << point << " " << remove_length << " " << how_many_lines(s) << endl
               << s;
            return ss.str();
        }
        void run_memory_test();
        void wait(int milli){
            boost::this_thread::sleep(boost::posix_time::milliseconds(milli));
        }
        void run_test(){
            run_memory_test();
        }
        void run_memory_test(){
            stringstream in,out;
            PipeServer ps(in,cerr);
            // donot start.
//            string tosend = "#+f(100000) -> 55;\n"
//                            "let f = \\(n){\n"
//                            "  if n = 0 {\n"
//                            "    return 0\n;"
//                            "  } else { \n"
//                            "    return n * f(n-1);\n"
//                            "}};\n";
//
            string tosend = "#+f(100000) -> 55;\n"
                            "let f = \\(n){\n"
                            "  mut ret = 0;\n"
                            "  for i in [1..n] { ret <- ret + i;}\n"
                            "  return ret;"
                            "};\n";


            ps.on_change_command(make_change(1,0,tosend));
            for(int i=0;i<30000;i++){
                cerr << i << endl;
                ps.on_change_command(make_change(1,tosend.size(),tosend));
                wait(1000);
            }
            cerr << "check memory state" << endl;
            wait(5000);

        }
    }
}

