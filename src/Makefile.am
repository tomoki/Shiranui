include $(top_srcdir)/flymake.mk
bin_PROGRAMS = shiranui
shiranui_SOURCES = main.cpp \
                   syntax/ast.cpp \
                   runtime/value.cpp \
                   runtime/runner.cpp \
                   runtime/environment.cpp \
                   server/server.cpp \
                   runtime/diver/diver.cpp \
                   runtime/cleaner.cpp \
                   tester/tester.cpp \
                   runtime/dsl/dsl_runtime.cpp
shiranui_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LIB) $(BOOST_SYSTEM_LIB) $(BOOST_THREAD_LIB)
shiranui_CXXFLAGS = -Wall -Wextra
