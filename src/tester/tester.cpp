#include "tester.hpp"
#include "../server/server.hpp"
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
        void run_memory_test();
        void wait(int milli){
            boost::this_thread::sleep(boost::posix_time::milliseconds(milli));
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
            for(int i=0;i<10;i++){
                const auto start_time = std::chrono::system_clock::now();
                pos_iterator_t first(long_code.begin()),last(long_code.end());
                pos_iterator_t iter = first;
                sp<ast::SourceCode> program;
                bool ok = false;
                try{
                    Parser<pos_iterator_t> resolver;
                    ok = boost::spirit::qi::phrase_parse(iter,last,resolver,boost::spirit::qi::space,program);
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
        void run_test(){
            //run_memory_test();
            parser_time_test();
        }

    }
}

