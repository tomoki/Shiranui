# Programming Language Shiranui

Combine LiveProgramming and Design-by-Contract.
On the fly testcase making.

![shiranui logo](logo_small.png)

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


# How to run

At first,run shiranui to see help.

```
$ src/shiranui -h
No arguments options:
  -h [ --help ]         print help message
  -s [ --server ]       run Shiranui in server mode
  -k [ --kagero ] arg   Specify Kagero(Standard) library path
  -e [ --exec ] arg     execute given file
  -c [ --compile ] arg  compile file to c++
  -a [ --arare ] arg    Specify Arare(launchpad) path
  -o [ --out ] arg      Specify output file
  -t [ --test ]         test(donot use.)
```

## run in IDE(Emacs)
Shiranui's primary development environment is Emacs.
At first,eval kasumi/kasumi.el (use "M-x eval-current-buffer").
Next,open main.nui and run "M-x kasumi-mode".

Shiranui will run in server mode and see how code changes.
Remember,Shiranui doesn't support UTF-8 currently,so DO NOT type unicode.

Opening doc/demo.nui is helpful.

## compile it.
Shiranui has shiranui to C++ compiler.
But,some function in shiranui is not supported

- clojure (function + environment)

Remember,compiled file may lack "main".
If you want to define main in Shiranui,use following.

```
let main = \(){
    let unit = print("helloworld");
};
```

```
$ src/shiranui -c doc/compiler_test.shi -a lib/arare.cpp -o test.cpp
$ clang++ test.cpp -std=c++1y -o a.out
$ ./a.out
```

## run REPL(read-eval-print-loop)

```
$ src/shiranui
This is shiranui 1.0.0
> let a = 1;
let a = 1;
1
> let unit = system_call("print")(a);
let unit = system_call(print)(a);
1
1
> good bye shirei
```

"system_call" stands for call bulitin function.
Currenty, Shiranui doesn't have type.



