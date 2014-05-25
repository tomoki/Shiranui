#include <iostream>
#include <fstream>
#include <config.hpp>
#include "misc.hpp"
#include "syntax/parser.hpp"



void test(const std::string content){
    typedef boost::spirit::line_pos_iterator<std::string::const_iterator>
        pos_iterator_t;

    using namespace shiranui;
    using namespace shiranui::syntax;
    pos_iterator_t first(content.begin()),
        iter = first,last(content.end());
    Parser<pos_iterator_t> resolver(first);
    // Parser<pos_iterator_t> p(first);
    ast::SourceCode* program;
    bool ok = qi::phrase_parse(iter,last,resolver,qi::space,program);
    std::cout << "ok: " << ok << std::endl;
    std::cout << "full: " << (iter == last) << std::endl;

    if(ok and iter == last){
        std::cout << "OK: parsing success" << std::endl;
        for(const auto& a : program->statements){
            std::cout << *a << std::endl;
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
    return;
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
        std::string str;
        std::getline(std::cin,str);
        test(str);
    }
    return 0;
}
