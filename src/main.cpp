#include <iostream>
#include <fstream>
#include <config.hpp>
#include <chrono>
#include <boost/program_options.hpp>
#include "misc.hpp"
#include "syntax/parser.hpp"
#include "runtime/runner.hpp"
#include "server/server.hpp"

void repl(){
    using namespace shiranui;
    using namespace shiranui::syntax;
    using namespace shiranui::runtime;
    shiranui::runtime::Runner r;
    shiranui::syntax::ast::PrettyPrinterForAST printer(std::cerr);
    shiranui::runtime::value::PrettyPrinterForValue printer_for_value(std::cerr);
    std::cerr << "This is " << PACKAGE_STRING << std::endl;
    while(true){
        std::cout << "> ";
        std::string str;
        std::getline(std::cin,str);
        if(std::cin.eof()==1) {
            std::cerr << "good bye shirei" << std::endl;
            return;
        }
        pos_iterator_t first(str.begin()),last(str.end());
        pos_iterator_t iter = first;
        ast::SourceCode* program;
        Parser<pos_iterator_t> resolver(first);
        bool ok = boost::spirit::qi::phrase_parse(iter,last,resolver,boost::spirit::qi::space,program);
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
        free(program);
    }
}

void start_server(){
    shiranui::server::PipeServer server(std::cin,std::cout);
    server.start();
}

int main(int argc,char **argv){
    namespace po = boost::program_options;
    po::options_description opt("Allowed options");
    opt.add_options()
        ("help,h","print help message")
        ("server,s","run Shiranui in server mode")
    ;
    po::variables_map vm;
    po::store(po::parse_command_line(argc,argv,opt),vm);
    po::notify(vm);
    if (vm.count("help")){
        std::cout << opt << std::endl;
        return 1;
    }
    if(vm.count("server")){
        start_server();
    }else{ // repl
        repl();
    }

    return 0;
}
