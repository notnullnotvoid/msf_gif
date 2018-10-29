gcc-8 -std=c99 -Wall -Ofast -c giff.c -o giff.o || exit
gcc-8 -std=c++11 -Wall -Ofast -c main.cpp -o main.o || exit
gcc-8 giff.o main.o -o giff -lstdc++ -lSDL2 || exit
rm giff.o main.o
./giff
# clang -std=c++11 -Wall giff.c -E > out.cpp
