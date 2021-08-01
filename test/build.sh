#!/usr/bin/env sh
cd "$(dirname "$0")"

#C++ config (w/profiling)
clang -march=native -std=c++17 -Wall -Os -c msf_gif.cpp -o msf_gif.o || exit
clang -march=native -std=c++17 -Wall -Os -c trace.cpp -o trace.o || exit
clang -march=native -std=c++17 -Wall -Os -c test.cpp -o test.o || exit
clang msf_gif.o trace.o test.o -o giff -lstdc++ || exit
rm test.o trace.o msf_gif.o

#C config (no profiling)
# clang -msse3 -std=c99   -Wall -Os -c msf_gif.c -o msf_gif.o || exit
# clang -msse3 -std=c++17 -Wall -Os -c trace.cpp -o trace.o || exit
# clang -msse3 -std=c++17 -Wall -Os -c test.cpp -o test.o || exit
# clang msf_gif.o trace.o test.o -o giff -lstdc++ || exit
# rm test.o trace.o msf_gif.o

# clang -march=native -std=c++17 -Wall -O0 -g -fsanitize=address -c msf_gif.cpp -o msf_gif.o || exit
# clang -march=native -std=c++17 -Wall -O0 -g -fsanitize=address -c trace.cpp -o trace.o || exit
# clang -march=native -std=c++17 -Wall -O0 -g -fsanitize=address -c test.cpp -o test.o || exit
# clang msf_gif.o trace.o test.o -g -fsanitize=address -o giff -lstdc++ || exit

# clang -march=native -std=c++17 -Wall -O0 -g -c msf_gif.cpp -o msf_gif.o -fsanitize=address || exit
# clang -march=native -std=c++17 -Wall -O0 -g -c trace.cpp -o trace.o -fsanitize=address || exit
# clang -march=native -std=c++17 -Wall -O0 -g -c test.cpp -o test.o -fsanitize=address || exit
# clang msf_gif.o trace.o test.o -o giff -lstdc++ -fsanitize=address || exit

./giff
# clang -std=c++11 -Wall giff.c -E > out.cpp
