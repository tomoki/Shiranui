#ifndef SERVER_HPP_INCLUDED
#define SERVER_HPP_INCLUDED

#include <iostream>
#include <mutex>
#include <thread>
#include "../runtime/runner.hpp"
#include "../syntax/parser.hpp"
#include "../runtime/value.hpp"

namespace shiranui{
    namespace server{
        int how_many_lines(const std::string&);
        struct PipeServer{
            std::istream& is;
            std::ostream& os;
            std::mutex send_lock;
            sp<syntax::ast::SourceCode> program;
            runtime::Runner current_runner;
            std::string source;
            PipeServer(std::istream&,std::ostream&);
            void start();
            void send_command(const std::string&,const std::string&);
            void send_simple_command(const std::string&,const int&,const int&);
            void send_good_flyline(const int&,const int&);
            void send_bad_flyline(const int&,const int&);
            void send_syntaxerror(const int&,const int&);
            void receive_command();
            virtual void on_receive_command(const std::string&,const std::string&);
            void on_receive_load_command(std::string);
            void send_run_flyline();
        };
    }
}
#endif
