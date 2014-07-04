include $(top_srcdir)/flymake.mk
bin_PROGRAMS = shiranui
shiranui_SOURCES = main.cpp syntax/ast.cpp runtime/value.cpp runtime/runner.cpp runtime/environment.cpp server/server.cpp runtime/diver/diver.cpp runtime/cleaner.cpp tester/tester.cpp
shiranui_LDFLAGS = -lboost_program_options -lboost_system -lboost_thread
shiranui_CXXFLAGS = -Wall
