# Programming Language Shiranui

Combine LiveProgramming and Design-by-Contract.
On the fly testcase making.

# How to build

You must have following softwares installed.
- Autotools(autoconf,automake)
- C++ Compiler that supports c++11(I recommend gcc or clang)

At first,cd to shiranui dir.

```
$ autoreconf
$ automake --add-missing
$ autoreconf
$ ./configure
$ make
```

