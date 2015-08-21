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
        const std::string COMMAND_CHANGE = "change";
        const std::string COMMAND_DIVE = "dive";
        const std::string COMMAND_FLYMARK_JUMP = "flymark_jump";
        const std::string COMMAND_SURFACE = "surface";
        const std::string COMMAND_MOVE_TO_CALLER = "move_to_caller";
        const std::string COMMAND_LIFT = "lift";
        const std::string COMMAND_DEBUG_PRINT = "debug";
        const std::string COMMAND_SYNTAXEROR = "syntaxerror";
        const std::string COMMAND_RUNTIMEERROR = "runtimeerror";
        const std::string COMMAND_IDLE_FLYLINE = "idleflyline";
        const std::string COMMAND_GOOD_FLYLINE = "goodflyline";
        const std::string COMMAND_BAD_FLYLINE = "badflyline";
        const std::string COMMAND_DIVE_STRIKE = "dive_strike";
        const std::string COMMAND_DIVE_HIGHLIGHT = "dive_highlight";
        const std::string COMMAND_DIVE_EXPLORE = "dive_explore";
        const std::string COMMAND_DIVE_CLEAR = "dive_clear";
        const std::string COMMAND_LOCK_FLYLINE = "lock_flyline";
        const std::string COMMAND_FLYMARK = "flymark_result";
        const std::string COMMAND_FLYMARK_INDEX = "flymark_index";
        const std::string COMMAND_LIFT_RESULT = "lift_result";
        const int FLYLINE_LOCK_FREE = -1;
        int how_many_lines(const std::string&);


        struct PipeServer{
            std::istream& is;
            std::ostream& os;
            boost::mutex os_lock;
            std::string source;
            int flyline_lock;
            runtime::MemoryManager mem_manager;

            runtime::MemoryManager mem_manager_for_toplevel;
            runtime::Memory* previous_mem_for_toplevel;

            boost::thread main_thread;
            //boost::mutex main_thread_waiting_mutex;
            //boost::condition main_thread_waiting;

            std::vector<std::shared_ptr<boost::thread>> flyline_threads;
            std::vector<sp<syntax::ast::SourceCode>> program_per_flyline;
            std::vector<std::shared_ptr<runtime::diver::Diver>> diver_per_flyline;
            std::vector<runtime::Memory*> previous_memorys;
            std::shared_ptr<runtime::diver::Diver> current_diver;


            PipeServer(std::istream&,std::ostream&);
            ~PipeServer();

            void start();
            void send_command(const std::string&,const std::string&,const int);
            void send_command_with_two_points(const std::string&,const int,const int,
                                              const int);

            void send_syntaxerror(const int,const int,const int);
            void send_runtimeerror(const int,const int,const int);
            void send_lock_flyline(const int,const int,const int);
            void send_good_flyline(const int,const int,const int,
                                   const int,const int);
            void send_bad_flyline(const int,const int,const int,
                                  const int,const std::string&,const int);
            void send_idle_flyline(const int,const int,const int,
                                   const int,const std::string&,const int);

            // void send_flymark(const int,const int,const int,
            //                   const int,const std::string&,const int);

            template<typename T>
            void send_debug_print(const T&,const int);
            void send_dive_strike(const int,const int,const int);
            void send_dive_highlight(const int,const int,const int);
            void send_dive_explore(const int,const int,const std::string&,const int);
            void send_dive_flymark_result(const int,const int,const int,
                                          const int,const std::string&,const int);
            void send_dive_flymark_index(const int,const int,const int,const int);
            void send_dive_clear(const int);
            void send_dive_lift_result(const std::string,const int);

            void receive(); // run as thread.
            void receive_command(const std::string&,const std::string&,const int);
            void on_change_command(const std::string&,const int);
            void on_dive_command(const std::string&,const int);
            void on_surface_command(const std::string&,const int);
            void on_move_to_caller_command(const std::string&,const int);
            void on_lift_command(const std::string&,const int);
            void on_jump_command(const std::string&,const int);
            void exec(std::string,const int);
            void dive_start(std::shared_ptr<runtime::diver::Diver>,sp<syntax::ast::FlyLine>,
                            sp<syntax::ast::SourceCode>,const int);
            void dive(std::shared_ptr<runtime::diver::Diver>,int,const int);
            void surface(std::shared_ptr<runtime::diver::Diver>,int);
            void move_to_caller(std::shared_ptr<runtime::diver::Diver>,int);
            void send_diving_message(runtime::diver::DivingMessage,int);
            void run_flyline(runtime::Memory*,std::string,int,const int);
            void run_testflyline(runtime::Runner&,sp<syntax::ast::TestFlyLine>,const int);
            void run_idleflyline(runtime::Runner&,sp<syntax::ast::IdleFlyLine>,
                                 sp<syntax::ast::SourceCode>,const int);
        };
    }
}
#endif
