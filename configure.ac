AC_CONFIG_MACRO_DIR([m4])
AC_INIT([shiranui],[1.0.0],[tomo832@gmail.com])
AM_INIT_AUTOMAKE([subdir-objects foreign -Wall -Werror -Wno-portability])
# for better message,use https://www.gnu.org/software/autoconf-archive/ax_cxx_compile_stdcxx_11.html#ax_cxx_compile_stdcxx_11
CXXFLAGS="$CXXFLAGS -std=c++11 -O2"
AC_LANG([C++])
AC_PROG_CXX
AX_BOOST_BASE
AX_BOOST_PROGRAM_OPTIONS
AX_BOOST_SYSTEM
AX_BOOST_THREAD
AC_CONFIG_HEADERS([config.hpp])
AC_CONFIG_FILES([Makefile src/Makefile])
AC_OUTPUT
