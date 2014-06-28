#ifndef SERVER_HPP_INCLUDED
#define SERVER_HPP_INCLUDED

#include <iostream>
#include <boost/thread/thread.hpp>

#include "../runtime/runner.hpp"
#include "../syntax/parser.hpp"
#include "../runtime/value.hpp"

namespace shiranui{
    namespace server{
        int how_many_lines(const std::string&);
        struct PipeServer{
            std::istream& is;
            std::ostream& os;
            boost::mutex os_lock;
            std::string source;

            boost::thread main_thread;
            boost::thread_group flyline_threads;
            std::vector<sp<syntax::ast::SourceCode>> program_per_flyline;

            PipeServer(std::istream&,std::ostream&);

            void start();
            void send_command(const std::string&,const std::string&);
            void send_command_with_two_points(const std::string&,const int&,const int&);
            void send_syntaxerror(const int&,const int&);
            void send_runtimeerror(const int&,const int&);
            void send_good_flyline(const int&,const int&);
            void send_bad_flyline(const int&,const int&);
            void send_idle_flyline(const int&,const int&, const int&,const int&,
                                   const int&,const std::string&);

            void send_debug_print(const std::string&);

            void receive(); // run as thread.
            void receive_command(const std::string&,const std::string&);
            void on_change_command(const std::string&);
            void exec(std::string);
            void run_flyline(std::string,int);
            void run_testflyline(std::string,runtime::Runner,sp<syntax::ast::TestFlyLine>);
            void run_idleflyline(std::string,runtime::Runner,sp<syntax::ast::IdleFlyLine>);
        };
    }
}
#endif
