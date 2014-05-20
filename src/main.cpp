#include <iostream>
#include <config.hpp>

int main(int argc,char **argv){
    std::cout << "Hello World" << std::endl;
    std::cout << "This is " << PACKAGE_STRING << std::endl;
    return 0;
}
