@echo off
del giff.exe

rem C++ config (w/profiling)
REM clang -march=native -std=c++17 -Wno-deprecated-declarations -Wall -Os -c msf_gif.cpp -o msf_gif.o
REM clang -march=native -std=c++17 -Wno-deprecated-declarations -Wall -Os -c trace.cpp -o trace.o
REM clang -march=native -std=c++17 -Wno-deprecated-declarations -Wall -Os -c test.cpp -o test.o
REM clang msf_gif.o trace.o test.o -o giff.exe

rem C config (no profiling)
REM clang -march=native -Wno-deprecated-declarations -std=c99 -Wall -Os -c msf_gif.c -o msf_gif.o
REM clang -march=native -Wno-deprecated-declarations -std=c++17 -Wall -Os -c trace.cpp -o trace.o
REM clang -march=native -Wno-deprecated-declarations -std=c++17 -Wall -Os -c test.cpp -o test.o
REM clang msf_gif.o trace.o test.o -o giff.exe



rem MSVC++ config (w/profiling)
REM cl /c /Ox /std:c++17 /W2 msf_gif.cpp /Fo:msf_gif.o
REM cl /c /Ox /std:c++17 /W2 trace.cpp /Fo:trace.o
REM cl /c /Ox /std:c++17 /W2 test.cpp /Fo:test.o
REM cl msf_gif.o trace.o test.o /Fe:giff.exe

rem MSVC config (no profiling)
cl /c /O2 /W3 msf_gif.c /Fo:msf_gif.o
cl /c /O2 /W2 trace.cpp /Fo:trace.o
cl /c /O2 /W2 test.cpp /Fo:test.o
cl msf_gif.o trace.o test.o /Fe:giff.exe

del test.o trace.o msf_gif.o
giff.exe
