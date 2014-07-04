#ifndef SERVER_HPP_INCLUDED
#define SERVER_HPP_INCLUDED

#include <iostream>
#include <boost/thread/thread.hpp>

#include "../runtime/runner.hpp"
#include "../syntax/parser.hpp"
#include "../runtime/value.hpp"
#include "../runtime/diver/diver.hpp"

namespace shiranui{
    namespace server{
        int how_many_lines(const std::string&);
        struct PipeServer{
            std::istream& is;
            std::ostream& os;
            boost::mutex os_lock;
            std::string source;

            boost::thread main_thread;
            //boost::mutex main_thread_waiting_mutex;
            //boost::condition main_thread_waiting;

            std::vector<sp<boost::thread>> flyline_threads;
            std::vector<sp<syntax::ast::SourceCode>> program_per_flyline;
            std::vector<sp<runtime::diver::Diver>> diver_per_flyline;
            sp<runtime::diver::Diver> current_diver;

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

            template<typename T>
            void send_debug_print(const T&);
            void send_dive_strike(const int&,const int&);
            void send_dive_clear();

            void receive(); // run as thread.
            void receive_command(const std::string&,const std::string&);
            void on_change_command(const std::string&);
            void on_dive_command(const std::string&);
            void on_surface_command(const std::string&);
            void exec(std::string);
            void dive_start(sp<runtime::diver::Diver>,sp<syntax::ast::FlyLine>);
            void dive(sp<runtime::diver::Diver>,int);
            void surface(sp<runtime::diver::Diver>);
            void send_diving_message(const std::string& source,
                                     runtime::diver::DivingMessage message);
            void run_flyline(std::string,int);
            void run_testflyline(std::string,runtime::Runner,sp<syntax::ast::TestFlyLine>);
            void run_idleflyline(std::string,runtime::Runner,sp<syntax::ast::IdleFlyLine>);
        };
    }
}
#endif
