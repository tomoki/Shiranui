#include <iostream>
#include <fstream>
#include <config.hpp>
#include <chrono>
#include "misc.hpp"
#include "syntax/parser.hpp"
#include "runtime/runner.hpp"


void test(const std::string content){
    typedef boost::spirit::line_pos_iterator<std::string::const_iterator>
        pos_iterator_t;
    using namespace shiranui;
    using namespace shiranui::syntax;
    using namespace shiranui::runtime;
    const auto start_time = std::chrono::system_clock::now();
    pos_iterator_t first(content.begin()),iter = first,last(content.end());
    Parser<pos_iterator_t> resolver(first);
    ast::SourceCode* program;
    bool ok = qi::phrase_parse(iter,last,resolver,qi::space,program);
    const auto end_time = std::chrono::system_clock::now();
    const auto timespan = end_time - start_time;
    std::cout << "ok: " << ok << std::endl;
    std::cout << "full: " << (iter == last) << std::endl;
    std::cout << "time: " << std::chrono::duration_cast<std::chrono::milliseconds>(timespan).count() << "[ms]" << std::endl;

    if(ok and iter == last){
        std::cout << "OK: parsing success" << std::endl;
        ast::PrettyPrinterForAST pp(std::cout);
        program->accept(pp);
        Runner r;
        program->accept(r);
    }else{
        int line = get_line(iter);
        int column = get_column(first,iter);
        std::cout << "-------------------------\n";
        std::cout << "ERROR: Parsing failed or not complete\n";
        std::cout << "stopped at: " << line  << ":" << column << "\n";
        std::cout << "remaining: '" << std::string(iter, last) << "'\n";
        std::cout << "-------------------------\n";
    }
    return;
}

void repl(){
    using namespace shiranui;
    using namespace shiranui::syntax;
    using namespace shiranui::runtime;
    shiranui::runtime::Runner r;
    shiranui::syntax::ast::PrettyPrinterForAST printer(std::cerr);
    shiranui::runtime::value::PrettyPrinterForValue printer_for_value(std::cerr);
    while(true){
        std::cout << "> ";
        std::string str;
        std::getline(std::cin,str);
        if(std::cin.eof()==1) {
            std::cout << "good bye shirei" << std::endl;
            return;
        }
        pos_iterator_t first(str.begin()),last(str.end());
        pos_iterator_t iter = first;
        ast::SourceCode* program;
        Parser<pos_iterator_t> resolver(first);
        bool ok = qi::phrase_parse(iter,last,resolver,qi::space,program);
        if(ok and iter == last){
            program->accept(printer);
            try{
                program->accept(r);
                r.cur.v->accept(printer_for_value);
                std::cerr << std::endl;
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
            }
        }else{
            int line = get_line(iter);
            int column = get_column(first,iter);
            std::cout << "-------------------------\n";
            std::cout << "ERROR: Parsing failed or not complete\n";
            std::cout << "stopped at: " << line  << ":" << column << "\n";
            std::cout << "remaining: '" << std::string(iter, last) << "'\n";
            std::cout << "-------------------------\n";
        }
    }
}

int main(int argc,char **argv){
    std::cout << "Hello World" << std::endl;
    std::cout << "This is " << PACKAGE_STRING << std::endl;

    if(argc == 2){
        std::string filename = argv[1];
        std::ifstream ifs(filename);
        std::string str((std::istreambuf_iterator<char>(ifs)),
                        std::istreambuf_iterator<char>());
        test(str);
    }else{
        repl();
    }
    return 0;
}
