rem C++ config (w/profiling)
clang -march=native -std=c++17 -Wno-deprecated-declarations -Wall -Os -c msf_gif.cpp -o msf_gif.o
clang -march=native -std=c++17 -Wno-deprecated-declarations -Wall -Os -c trace.cpp -o trace.o
clang -march=native -std=c++17 -Wno-deprecated-declarations -Wall -Os -c test.cpp -o test.o
clang msf_gif.o trace.o test.o -o giff
del test.o trace.o msf_gif.o

rem C config (no profiling)
REM clang -march=native -Wno-deprecated-declarations -std=c99 -Wall -Os -c msf_gif.c -o msf_gif.o
REM clang -march=native -Wno-deprecated-declarations -std=c++17 -Wall -Os -c trace.cpp -o trace.o
REM clang -march=native -Wno-deprecated-declarations -std=c++17 -Wall -Os -c test.cpp -o test.o
REM clang msf_gif.o trace.o test.o -o giff.exe
REM del test.o trace.o msf_gif.o

giff.exe
