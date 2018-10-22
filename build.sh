gcc-8 -std=c++11 -Wall -Ofast -o giff main.cpp giff.cpp -lstdc++ -lSDL2 || exit
./giff
# clang -std=c++11 -Wall giff.cpp -E > out.cpp
