#include "tester.hpp"
#include "../server/server.hpp"
#include "../runtime/value_printer.hpp"

#include <iostream>
#include <chrono>

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
        void run_program(std::string str){
            using namespace shiranui;
            using namespace shiranui::syntax;
            using namespace shiranui::runtime;

            Memory* memory = new Memory(268435456);
            shiranui::runtime::Runner r(memory);
            shiranui::syntax::ast::PrettyPrinterForAST printer(std::cerr);

            pos_iterator_t first(str.begin()),last(str.end());
            pos_iterator_t iter = first;
            sp<ast::SourceCode> program;
            bool ok = false;
            try{
                Parser<pos_iterator_t> resolver(memory);
                ok = parse(iter,last,resolver,program);
            }catch (boost::spirit::qi::expectation_failure<pos_iterator_t> const& x){
                std::cerr << "expected: ";
                std::cerr << x.what_ << std::endl;
                std::cerr << "got: \"" << std::string(x.first, x.last) << '"' << std::endl;
            }
            if(ok and iter == last){
                program->accept(printer);
                try{
                    program->accept(r);
                    std::cerr << runtime::value::to_reproductive(r.cur_v) << std::endl;
                }catch(NoSuchVariableException e){
                    std::cerr << "No such variable: ";
                    e.where->accept(printer);
                    std::cerr << std::endl;
                }catch(ConvertException e){
                    std::cerr << "Convert Error: ";
                    e.where->accept(printer);
                    std::cerr << std::endl;
                }catch(RuntimeException e){
                    std::cerr << "Something RuntimeException: ";
                    e.where->accept(printer);
                    std::cerr << std::endl;
                }catch(MemoryLimitException e){
                    std::cerr << "memory over" << std::endl;
                }
            }else{
                std::cerr << "parse failed" << std::endl;
            }
            memory->destruct_all();
        }
        void wait(int milli){
            boost::this_thread::sleep(boost::posix_time::milliseconds(milli));
        }
        void check_leak(std::string str){
            using namespace shiranui;
            using namespace shiranui::syntax;
            using namespace shiranui::runtime;

            Memory* memory = new Memory(268435456*3);

            for(int i=0;i<10;i++){
                shiranui::runtime::Runner r(memory);
                shiranui::syntax::ast::PrettyPrinterForAST printer(std::cerr);

                pos_iterator_t first(str.begin()),last(str.end());
                pos_iterator_t iter = first;
                sp<ast::SourceCode> program;
                bool ok = false;
                try{
                    Parser<pos_iterator_t> resolver(memory);
                    ok = parse(iter,last,resolver,program);
                }catch (boost::spirit::qi::expectation_failure<pos_iterator_t> const& x){
                    std::cerr << "expected: ";
                    std::cerr << x.what_ << std::endl;
                    std::cerr << "got: \"" << std::string(x.first, x.last) << '"' << std::endl;
                }
                if(ok and iter == last){
                    program->accept(printer);
                    try{
                        program->accept(r);
                        std::cerr << runtime::value::to_reproductive(r.cur_v) << std::endl;
                    }catch(NoSuchVariableException e){
                        std::cerr << "No such variable: ";
                        e.where->accept(printer);
                        std::cerr << std::endl;
                    }catch(ConvertException e){
                        std::cerr << "Convert Error: ";
                        e.where->accept(printer);
                        std::cerr << std::endl;
                    }catch(RuntimeException e){
                        std::cerr << "Something RuntimeException: ";
                        e.where->accept(printer);
                        std::cerr << std::endl;
                    }catch(MemoryLimitException e){
                        std::cerr << "memory over" << std::endl;
                    }
                }else{
                    std::cerr << "parse failed" << std::endl;
                }
                memory->destruct_all();
                memory->seek();
            }
        }

        void run_check_leak(){
            string tosend = "let f = \\(n){ if n <= 1 { return n; } else { return f(n-1) + f(n-2); }}; f(35);";
            check_leak(tosend);
        }
        void run_alloc_test(){
            string tosend = "let f = \\(n){ if n <= 1 { return n; } else { return f(n-1) + f(n-2); }}; f(30);";
            run_program(tosend);
        }
        void run_memory_test(){
            stringstream in,out;
            PipeServer ps(in,cerr);
            string tosend = "#+f(100000) -> 55;\n"
                            "let f = \\(n){\n"
                            "  let ret = ref 0;\n"
                            "  for i in [1..n] { ret <- !ret + i;}\n"
                            "  return ret;"
                            "};\n";

            ps.on_change_command(make_change(1,0,tosend),1);
            for(int i=0;i<30000;i++){
                cerr << i << endl;
                ps.on_change_command(make_change(1,tosend.size(),tosend),1);
                wait(1000);
            }
            cerr << "check memory state" << endl;
            wait(5000);
        }
        void run_memory_test3(){
            stringstream in;
            PipeServer ps(in,cerr);
            string tosend = "#+ f(24) -> ; #+ f(10) -> ; #+ f(5) -> ; #+f(25) -> 55; #+f(30) -> 55;\n"
                "let f = \\(n){ if n <= 1 { return n; } else { return f(n-1) + f(n-2); }};";

            ps.on_change_command(make_change(1,0,tosend),1);
            wait(1000);
            for(int i=0;i<3;i++){
                cerr << "t:" << i << " m:" << ps.mem_manager.chunks.size() << endl;
                ps.on_change_command(make_change(1,tosend.size(),tosend),1);
                wait(300);
            }
            cerr << "check memory state" << endl;
            wait(5000);
        }
        void run_memory_test2(){
            stringstream in;
            PipeServer ps(in,cerr);
            string tosend = "#+ f(40) -> ; #+ f(10) -> ; #+ f(5) -> ; #+f(30) -> 55;\n"
                "let f = \\(n){ if n <= 1 { return n; } else { return f(n-1) + f(n-2); }};";

            ps.on_change_command(make_change(1,0,tosend),1);
            for(int i=0;i<50;i++){
                cerr << i << endl;
                ps.on_change_command(make_change(1,tosend.size(),tosend),1);
                wait(2000);
            }
            cerr << "check memory state" << endl;
            wait(5000);
        }
        void run_memory_test4(){
            stringstream in;
            PipeServer ps(in,cerr);
            string tosend = "let f = \\(n){ if n <= 1 { return n; } else { return f(n-1) + f(n-2); }}; f(35);";
            ps.on_change_command(make_change(1,0,tosend),1);
            wait(100);
            for(int i=0;i<5;i++){
                cerr << i << endl;
                ps.on_change_command(make_change(1,tosend.size(),tosend),1);
                wait(1000);
            }
            cerr << "check memory state" << endl;
            wait(100);
        }
        void parser_time_test(){
            using namespace syntax;
            using namespace syntax::ast;
string long_code = 
"let unit = print(true);\n"
"let unit = print(false);\n"
"let unit = print(1);\n"
"let unit = print(\"Shiranui\");\n"
"let sum = \\(n){\n"
"    mut a = 0;\n"
"    for i in [0..n-1] {\n"
"        a <- a + i;\n"
"    }\n"
"    return a;\n"
"};\n"
"\n"
"let unit = print(sum(1));\n"
"let unit = print(sum(2));\n"
"let unit = print(sum(3));\n"
"let unit = print(sum(4));\n"
"let f = \\(n){\n"
"    for i in [1..n]{\n"
"        if i = n/2 {\n"
"            return i;\n"
"        }\n"
"    }\n"
"    return 0;\n"
"};\n"
"\n"
"let unit = print(f(2));\n"
"let unit = print(f(4));\n"
"let unit = print(f(0));\n"
"\n"
"let unit = print(\"Hello\");\n"
"let unit = print(\"Hello\" + \"World\");\n"
"let unit = print(\"Hello\" = \"Hello\");\n"
"let unit = print(\"Hello\" = \"World\");\n"
"let unit = print(\"Hello\" /= \"Hello\");\n"
"let unit = print(\"Hello\" /= \"World\");\n"
"let unit = print(1+2);\n"
"let unit = print(1-2);\n"
"let unit = print(2*3);\n"
"let unit = print(3/2);\n"
"\n"
"let unit = print(-1);\n"
"let unit = print(--1);\n"
"let unit = print(1 < 1);\n"
"let unit = print(1 <= 1);\n"
"let unit = print(1 = 1);\n"
"let unit = print(1 /= 1);\n"
"let unit = print(1 > 1);\n"
"let unit = print(1 >= 1);\n"
"\n"
"let unit = print(2 < 1);\n"
"let unit = print(2 <= 1);\n"
"let unit = print(2 = 1);\n"
"let unit = print(2 /= 1);\n"
"let unit = print(2 > 1);\n"
"let unit = print(2 >= 1);\n"
"\n"
"let unit = print(2 <  3);\n"
"let unit = print(2 <= 3);\n"
"let unit = print(2 =  3);\n"
"let unit = print(2 /= 3);\n"
"let unit = print(2 >  3);\n"
"let unit = print(2 >= 3);\n"
"let unit = print(not true);\n"
"let unit = print(not false);\n"
"let unit = print(true or true);\n"
"let unit = print(true or false);\n"
"let unit = print(false or true);\n"
"let unit = print(false or false);\n"
"let unit = print(true and true);\n"
"let unit = print(true and false);\n"
"let unit = print(false and true);\n"
"let unit = print(false and false);\n"
"let f = \\(n){\n"
"    return \\(){\n"
"        return n;\n"
"    };\n"
"};\n"
"\n"
"let unit = print(f(3)());\n"
"let f = \\(n){\n"
"    if n = 1 {\n"
"        return 1;\n"
"    } else {\n"
"        return 1 + f(n-1);\n"
"    }\n"
"};\n"
"\n"
"let unit = print(f(10));\n"
"let f = \\(){\n"
"    mut a = 0;\n"
"    let g = \\(){\n"
"        a <- 3;\n"
"        return 0;\n"
"    };\n"
"    let unused = g();\n"
"    return a;\n"
"};\n"
"\n"
"let unit = print(f());\n"
"let is_odd = \\(n){\n"
"    if n = 0 {\n"
"        return false;\n"
"    }else{\n"
"        return is_even(n-1);\n"
"    }\n"
"};\n"
"let is_even = \\(n){\n"
"    if n = 0 {\n"
"        return true;\n"
"    }else{\n"
"        return is_odd(n-1);\n"
"    }\n"
"};\n"
"\n"
"let unit = print(is_odd(20));\n"
"let unit = print(is_even(20));\n"
"let unit = print(is_odd(3));\n"
"let unit = print(is_even(3));\n"
"let gen = \\(){\n"
"    mut n = 0;\n"
"    let a = \\(){\n"
"        n <- n + 1;\n"
"        return n;\n"
"    };\n"
"    return a;\n"
"};\n"
"\n"
"let a = gen();\n"
"let unit = print(a());\n"
"let unit = print(a());\n"
"let unit = print(a());\n"
"let b = gen();\n"
"let unit = print(b());\n"
"let f = \\(n){\n"
"    return n;\n"
"};\n"
"\n"
"let unit = print(f(1));\n"
"let unit = print(f(\"Hi\"));\n"
"let fib = \\(n){\n"
"    if n = 0 or n = 1 {\n"
"        return 1;\n"
"    }else{\n"
"        return fib(n-1) + fib(n-2);\n"
"    }\n"
"};\n"
"\n"
"let unit = print(fib(0));\n"
"let unit = print(fib(1));\n"
"let unit = print(fib(2));\n"
"let unit = print(fib(3));\n"
"let unit = print(fib(4));\n"
"let unit = print(fib(5));\n"
"let unit = print(fib(6));\n"
"let unit = print(fib(7));\n"
"let a = 1;\n"
"let ab = 1;\n"
"let a1 = 2;\n"
"mut a = 1;\n"
"mut b = a;\n"
"let unit = print(a = b);\n"
"a <- 3;\n"
"let unit = print(a);\n"
"let unit = print(b);\n"
"let sum = \\(ar){\n"
"    mut ret = 0;\n"
"    for i in ar {\n"
"        ret <- ret + i;\n"
"    }\n"
"    return ret;\n"
"};\n"
"\n"
"let unit = print(sum([]));\n"
"let unit = print(sum([1]));\n"
"let unit = print(sum([1,2]));\n"
"let unit = print(sum([1,2,1+1+1]));\n"
"let a = [1..10);\n"
"let b = [1,2..10);\n"
"let c = [10,9..1);\n"
"let d = [10..1);\n"
"\n"
"let sum = \\(ar){\n"
"    mut ret = 0;\n"
"    for i in ar {\n"
"        ret <- ret + i;\n"
"    }\n"
"    return ret;\n"
"};\n"
"\n"
"let unit = print(sum(a));\n"
"let unit = print(sum(b));\n"
"let unit = print(sum(c));\n"
"let unit = print(sum(d));\n"
"let f = \\(){\n"
"    if true {\n"
"        return true;\n"
"    } else {\n"
"        return false;\n"
"    }\n"
"};\n"
"\n"
"let g = \\(){\n"
"    if true {\n"
"        return true;\n"
"    }\n"
"    return false;\n"
"};\n"
"\n"
"let f2 = \\(){\n"
"    if false {\n"
"        return true;\n"
"    } else {\n"
"        return false;\n"
"    }\n"
"};\n"
"\n"
"let g2 = \\(){\n"
"    if false {\n"
"        return true;\n"
"    }\n"
"    return false;\n"
"};\n"
"\n"
"\n"
"let unit = print(f());\n"
"let unit = print(g());\n"
"let unit = print(f2());\n"
"let unit = print(g2());\n";
            for(int i=0;i<3;i++){
                long_code = long_code + long_code;
            }

            for(int i=0;i<5;i++){
                runtime::Memory* memory = new runtime::Memory(10000000);
                const auto start_time = std::chrono::system_clock::now();
                pos_iterator_t first(long_code.begin()),last(long_code.end());
                pos_iterator_t iter = first;
                sp<ast::SourceCode> program;
                bool ok = false;
                try{
                    Parser<pos_iterator_t> resolver(memory);
                    ok = parse(iter,last,resolver,program);
                    // ok = boost::spirit::qi::phrase_parse(iter,last,resolver,skipper,program);
                }catch (boost::spirit::qi::expectation_failure<pos_iterator_t> const& x){
                    std::cerr << "expected: ";
                    std::cerr << x.what_ << std::endl;
                    std::cerr << "got: \"" << std::string(x.first, x.last) << '"' << std::endl;
                }
                const auto end_time = std::chrono::system_clock::now();
                const auto time_span = end_time - start_time;
                cerr << "First parse:" << std::chrono::duration_cast<std::chrono::milliseconds>(time_span).count() << "[ms]" << endl;
            }
        }
        void run_dive_test(){
            stringstream in,out;
            PipeServer ps(in,cerr);
            string tosend = "#- fact(3) -> 6;"
                            "let fact = \\(n){"
                            "    if n = 0 {\n"
                            "        return 1;\n"
                            "    } else {\n"
                            "        return n * fact(n-1);\n"
                            "    }\n"
                            "};\n";

            int k = 0;
            auto w = [&k](){
                wait(100);
                std::cerr << "-----" << k++ << "-----" << std::endl;
            };
            // for(int i=0;i<tosend.length();i++){
            //     std::cerr << i << ":" << tosend[i] << std::endl;
            // }
            w();
            ps.on_change_command(make_change(1,0,tosend),1);
            w();
            ps.on_dive_command("3",1);
            w();
            ps.on_dive_command("99",1);
            w();
            ps.on_dive_command("99",1);
            w();
            ps.on_surface_command("",1);
            w();
            ps.on_surface_command("",1);
            w();
        }

        void run_jump_test(){
            stringstream in,out;
            PipeServer ps(in,cerr);
            std::string s = "#+ f(10) -> 3628800;\n"
                            "#+ f(3) -> 6;\n"
                            "let f = \\f(n){\n"
                            "    #* n -> 10,9,8,7,6,5,4,3,2,1;\n"
                            "    if n = 1{\n"
                            "        return 1;\n"
                            "    }else{\n"
                            "        return n * f(n-1);\n"
                            "    }\n"
                            "};";
            for(int i=0;i<s.length();i++){
                std::cerr << i << ":" << s[i] << std::endl;
            }
            int k = 0;
            auto w = [&k](){
                wait(100);
                std::cerr << "-----" << k++ << "-----" << std::endl;
            };
            ps.on_change_command(make_change(1,0,s),1);
            w();
            ps.on_dive_command("3",1);
            w();
            ps.on_dive_command("24",1);
            w();
            ps.on_dive_command("3",1);
            w();
            ps.on_dive_command("24",1);
            w();
            ps.on_dive_command("3",1);
            w();
            for(int i=0;i<7;i++){
                std::stringstream ss;
                ss << 69 << " " << i;
                ps.on_jump_command(ss.str(),1);
                w();
            }
            w();
        }
        void run_move_to_caller_test(){
            stringstream in,out;
            PipeServer ps(in,cerr);
            std::string s = "#+ f(10) -> 3628800;\n"
                            "#+ f(3) -> 6;\n"
                            "let f = \\f(n){\n"
                            "    #* n -> 10,9,8,7,6,5,4,3,2,1;\n"
                            "    if n = 1{\n"
                            "        return 1;\n"
                            "    }else{\n"
                            "        return id(n) * f(n-1);\n"
                            "    }\n"
                            "};\n"
                            "let id = \\(n){return n;};";
            for(int i=0;i<s.length();i++){
                std::cerr << i << ":" << s[i] << std::endl;
            }
            int k = 0;
            auto w = [&k](){
                wait(100);
                std::cerr << "-----" << k++ << "-----" << std::endl;
            };
            ps.on_change_command(make_change(1,0,s),1);
            w();
            ps.on_dive_command("3",1);
            w();
            ps.on_jump_command("69 5",1);
            w();
            ps.on_move_to_caller_command("",1);
            w();
            ps.on_dive_command("143",1);
            w();
            ps.on_move_to_caller_command("",1);
            w();
        }
        void run_bad_dive_test(){
            stringstream in,out;
            PipeServer ps(in,cerr);
            string tosend = "#- hoge() -> 3;\n"
                            "let hoge = \\(){return n;};";
            ps.on_change_command(make_change(1,0,tosend),1);
            wait(100);
            ps.on_dive_command("2",1);
            wait(100);
            for(int i=0;i<3;i++){
                ps.on_change_command(make_change(tosend.length()-1+i,0,"\n"),2+i);
                wait(10);
            }
        }
        void run_good_dive_test(){
            stringstream in,out;
            PipeServer ps(in,cerr);
            string tosend = "#- hoge() -> 3;\n"
                            "let hoge = \\(){return 5;};";
            ps.on_change_command(make_change(1,0,tosend),1);
            wait(100);
            ps.on_dive_command("2",1);
            wait(100);
            for(int i=0;i<3;i++){
                ps.on_change_command(make_change(tosend.length()-1+i,0,"\n"),2+i);
                wait(10);
            }
        }
        void run_dive_tri(){
            stringstream in,out;
            PipeServer ps(in,cerr);
            string tosend = "#- tri(5) -> 9;\n"
                            "let tri = \\(n){\n"
                            "    if n = 0 or n = 1 or n = 2 {\n"
                            "        return 1;\n"
                            "    } else {\n"
                            "        return tri(n-1) + tri(n-2) + tri(n-3);\n"
                            "    }\n"
                            "};";

            run_program(tosend);
            ps.on_change_command(make_change(1,0,tosend),1);
            wait(100);
            cerr << "first" << endl;
            ps.on_dive_command("3",1);
            wait(100);
            cerr << "second" << endl;
            ps.on_dive_command("112",1);
            wait(100);
            //ps.on_surface_command("",1);
            //wait(100);
            //for(int i=0;i<tosend.size();i++){
            //    cerr << i << " : " << tosend[i] << endl;;
            //}
        }
        void run_bad_program(){
            std::string str = "let h = \\(){ return h();};\n"
                              "let unit = h();\n";
            run_program(str);
        }
        void run_flymark(){
            stringstream in,out;
            PipeServer ps(in,cerr);
            string tosend = "#- tri(5) -> 9;\n"
                            "let tri = \\(n){\n"
                            "    #* n -> ;\n"
                            "    if n = 0 or n = 1 or n = 2 {\n"
                            "        return 1;\n"
                            "    } else {\n"
                            "        return tri(n-1) + tri(n-2) + tri(n-3);\n"
                            "    }\n"
                            "};";

            run_program(tosend);
            ps.on_change_command(make_change(1,0,tosend),1);
            wait(100);
            cerr << "first" << endl;
            ps.on_dive_command("3",1);
            wait(100);
        }
        void run_zero_div(){
            std::string str = "let a = 0;let h = 1 / a;";
            run_program(str);
        }
        void run_plus(){
            std::string str = "let a = 1+1+1+1;";
            run_program(str);
        }
        void run_rec_test(){
            stringstream in,out;
            PipeServer ps(in,cerr);
            std::string str = "let get = system_call(\"get\");"
                              "let set = system_call(\"set\");"
                              "let rec = [1,2,3];"
                              "set(rec,2,rec);";
            run_program(str);
            ps.on_change_command(make_change(1,0,str),1);
            wait(100);
        }
        void run_rec_test2(){
            stringstream in,out;
            PipeServer ps(in,cerr);
            std::string str = "#+ <|a=[1,a]|> -> <|a=[1,2,a]|>;";
            run_program(str);
            ps.on_change_command(make_change(1,0,str),1);
            wait(100);
        }
        void run_lambda_man_test(){
            using namespace shiranui;
            using namespace shiranui::syntax;
            using namespace shiranui::runtime;

            std::string str = "let f = \\hogepoyo(){return 1;};";
            Memory* memory = new Memory(268435456);
            shiranui::runtime::Runner r(memory);
            pos_iterator_t first(str.begin()),last(str.end());
            pos_iterator_t iter = first;
            sp<ast::SourceCode> program;
            bool ok = false;
            Parser<pos_iterator_t> resolver(memory);
            ok = parse(iter,last,resolver,program);
            program->accept(r);
            dump(program->where_is_function_from.size(),std::cerr);
            dump(program->marker_to_lambda.size(),std::cerr);
        }

        void free_var(){
            using namespace shiranui;
            using namespace shiranui::syntax;
            using namespace shiranui::runtime;
            std::string str = "let add = \\(a){return \\inner(b){return a+b;};};\n"
                              "add(3);";
            Memory* memory = new Memory(268435456);
            shiranui::runtime::Runner r(memory);
            pos_iterator_t first(str.begin()),last(str.end());
            pos_iterator_t iter = first;
            sp<ast::SourceCode> program;
            Parser<pos_iterator_t> resolver(memory);
            parse(iter,last,resolver,program);
            program->accept(r);
            auto p = r.cur_v;
            auto f = dynamic_cast<value::UserFunction*>(r.cur_v);
            auto af = program->where_is_function_from[f->body];
            dump(f,std::cerr);
            dump(af,std::cerr);
            auto frees = scan_free_variable(af);
            std::cerr << "free variables -----" << std::endl;
            for(ast::Identifier i : frees){
                dump(i.name,std::cerr);
            }
            std::cerr << "--------------------" << std::endl;
            dump(*(f->env),std::cerr);
            auto filtered = filter_environment(*(f->env),frees);
            std::cerr << "filtered" << std::endl;
            for(auto p : filtered){
                std::cerr << p.first.name << " -> " << to_reproductive(p.second) << std::endl;
            }
        }
        void to_repr_test(){
            using namespace shiranui;
            using namespace shiranui::syntax;
            using namespace shiranui::runtime;
            std::vector<pair<string,string> > tests = {
                {"1;","1"},
                {"\"hoge\";","\"hoge\""},
                {"<|a=[a,1,1]|>;","<|a=[a,1,1]|>"},
                {"<|a=[[1,a],2]|>;","<|a=[[1,a],2]|>"},
                {"<|[[1,b=[1]],b]|>;","<|[[1,a],a=[1]]|>"}, // require bfs.
                {"\\id(){};","<|$()id|>"},
                {"{let g = \\b(n){return n;};let f = \\k(n){let b = g;return n;};f;}","<|$(g->$()b)k|>"},
                {"{let f = \\fa(n){return n*f(n-1);};f;}","<|a=$(f->a)fa|>"},
                {"{let add = \\t(a){return \\i(b){return add;};};add(3);}","<|$(add->a=$(add->a)t)i|>"},
                {"{let f = \\ff(){g();};let g = \\gg(){f();};f;}","<|a=$(g->$(f->a)gg)ff|>"},
                {"{let set = system_call(\"set\");let c = [1,2];let d = [3,4];set(c,0,d);set(d,1,c);let f = \\fid(){let cc = c;let dd = d;};}",
                 "<|$(c->a=[b,2],d->b=[3,a])fid|>"},
                {"let a = 1;let f = \\k(){return a;};f;","<|$()k|>"}, // global const variable should not be included
                {"mut a = 1;let f = \\k(){return a;};f;","<|$(a->ref 1)k|>"}
            };
            for(auto pair_of_source_and_ret : tests){
                std::string str = pair_of_source_and_ret.first;
                Memory* memory = new Memory(268435456);
                shiranui::runtime::Runner r(memory);
                pos_iterator_t first(str.begin()),last(str.end());
                pos_iterator_t iter = first;
                sp<ast::SourceCode> program;
                Parser<pos_iterator_t> resolver(memory);
                parse(iter,last,resolver,program);
                program->accept(r);
                auto p = r.cur_v;
                auto acc = to_reproductive(p,program);
                auto want = pair_of_source_and_ret.second;
                if(acc != want){
                    std::cerr << "----------ERROR----------" << std::endl;
                    dump(acc,std::cerr);
                    dump(want,std::cerr);
                    std::cerr << "-------------------------" << std::endl;
                }else{
                    dump(acc,std::cerr);
                }
            }
        }
        void run_versioning_test_array(){
            stringstream in,out;
            PipeServer ps(in,cerr);
            string s = "let get = system_call(\"get\");\n"
                       "let set = system_call(\"set\");\n"
                       "#+ f() -> [3];\n"
                       "let f = \\(){\n"
                       "    let ar = [0];\n"
                       "    set(ar,0,2);\n"
                       "    set(ar,0,3);\n"
                       "    set(ar,0,[0]);\n"
                       "    set(get(ar,0),0,123);\n"
                       "    set(get(ar,0),0,666);\n"
                       "    return ar;\n"
                       "};";
            for(int i=0;i<s.length();i++){
                std::cerr << i << ":" << s[i] << std::endl;
            }
            int k = 0;
            auto w = [&k](){
                wait(100);
                std::cerr << "-----" << k++ << "-----" << std::endl;
            };
            w();
            ps.on_change_command(make_change(1,0,s),1);
            w();
            ps.on_dive_command("64",1);
            w();
        }
        
        void run_versioning_test_closure(){
            stringstream in,out;
            PipeServer ps(in,cerr);
            string s = "let get = system_call(\"get\");\n"
                       "let set = system_call(\"set\");\n"
                       "#+ f() -> 0;\n"
                       "mut glob = 1;\n"
                       "let f = \\(){\n"
                       "    let ar = [[1],2,3];\n"
                       "    let g = \\k(){\n"
                       "        !glob;\n"
                       "        return ar;\n"
                       "    };\n"
                       "    g();\n"
                       "    glob <- [2];\n"
                       "    set(ar,1,99);\n"
                       "    g();\n"
                       "    set(!glob,0,567);\n"
                       "    set(get(ar,0),0,123);\n"
                       "    g();\n"
                       "    return 0;\n"
                       "};\n";

            for(int i=0;i<s.length();i++){
                std::cerr << i << ":" << s[i] << std::endl;
            }
            int k = 0;
            auto w = [&k](){
                wait(100);
                std::cerr << "-----" << k++ << "-----" << std::endl;
            };
            w();
            ps.on_change_command(make_change(1,0,s),1);
            w();
            ps.on_dive_command("64",1);
            w();
        }
        void ref_test1(){
            stringstream in,out;
            PipeServer ps(in,cerr);
            string s = "let a = ref 3;\n"
                       "let g = \\k(){\n"
                       "    return a;\n"
                      "};"
                     ;
            int k = 0;
            auto w = [&k](){
                wait(100);
                std::cerr << "-----" << k++ << "-----" << std::endl;
            };
            w();
            ps.on_change_command(make_change(1,0,s),1);
            w();
        }
        void run_complex_serialize(){
            stringstream in,out;
            PipeServer ps(in,cerr);
            std::string s = "let set = system_call(\"set\");\n"
                            "let get = system_call(\"get\");\n"
                            "\n"
                            "#+ p() -> <|$(c->[1,a],d->a=[3])h|>;\n"
                            "#+ <|$(c->[1,a],d->a=[3])h|> -> <|$(c->[1,a],d->a=[3])h|>;\n"
                            "\n"
                            "let p = \\(){\n"
                            "    let c = [1,2];\n"
                            "    let d = [3];\n"
                            "    set(c,1,d);\n"
                            "\n"
                            "    let h = \\h(){\n"
                            "        return get(c,0) + get(d,0);\n"
                            "    };\n"
                            "    return h;\n"
                            "};\n"
                          ;
            int k = 0;
            auto w = [&k](){
                wait(100);
                std::cerr << "-----" << k++ << "-----" << std::endl;
            };
            for(int i=0;i<50;i++){
                w();
                ps.on_change_command(make_change(1,0,s),i+1);
            }
        }
        void run_stackoverflow_test(){
            stringstream in;
            PipeServer ps(in,cerr);
            std::string s = "#+ f(10000) -> ;"
                            "let f = \\(n){ return f(n-1); };";
            ps.on_change_command(make_change(1,0,s),0);
            wait(1000);
        }
        void run_test(){
            run_stackoverflow_test();
            // run_alloc_test(); 
            // run_rec_test();
            // run_rec_test2();
            // run_lambda_man_test();
            // ref_test1();
            // to_repr_test();
            // free_var();
            // run_memory_test2();
            // run_memory_test3();
            // run_check_leak();
            // run_memory_test4();
            // parser_time_test();
            // run_dive_test();
            // run_jump_test();
            // run_move_to_caller_test();
            // run_bad_program();
            // run_zero_div();
            // run_dive_tri();
            // run_plus();
            // run_good_dive_test();
            // run_bad_dive_test();
            // run_flymark();
            // run_versioning_test_array();
            // run_versioning_test_closure();
            // run_complex_serialize();
        }
    }
}

