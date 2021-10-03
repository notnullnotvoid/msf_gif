@echo off
pushd "%~dp0"
    del giff.exe

    rem C++ config (w/profiling)
    clang -march=native -std=c++17 -Wno-deprecated-declarations -Wall -Os -c msf_gif.cpp -o msf_gif.o
    clang -march=native -std=c++17 -Wno-deprecated-declarations -Wall -Os -c trace.cpp -o trace.o
    clang -march=native -std=c++17 -Wno-deprecated-declarations -Wall -Os -c test.cpp -o test.o
    clang msf_gif.o trace.o test.o -o giff.exe

    rem C config (no profiling)
    rem clang -march=native -Wno-deprecated-declarations -std=c99 -Wall -Os -c msf_gif.c -o msf_gif.o
    rem clang -march=native -Wno-deprecated-declarations -std=c++17 -Wall -Os -c trace.cpp -o trace.o
    rem clang -march=native -Wno-deprecated-declarations -std=c++17 -Wall -Os -c test.cpp -o test.o
    rem clang msf_gif.o trace.o test.o -o giff.exe



    rem MSVC++ config (w/profiling)
    rem cl /c /Ox /std:c++17 /W3 msf_gif.cpp /Fo:msf_gif.o
    rem cl /c /Ox /std:c++17 /W2 trace.cpp /Fo:trace.o
    rem cl /c /Ox /std:c++17 /W2 test.cpp /Fo:test.o
    rem cl msf_gif.o trace.o test.o /Fe:giff.exe

    rem MSVC config (no profiling)
    rem cl /c /O2 /W3 msf_gif.c /Fo:msf_gif.o
    rem cl /c /O2 /W2 trace.cpp /Fo:trace.o
    rem cl /c /O2 /W2 test.cpp /Fo:test.o
    rem cl msf_gif.o trace.o test.o /Fe:giff.exe

    REM del test.o trace.o msf_gif.o
    giff.exe
popd
