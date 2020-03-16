# clang -march=native -std=c99 -Wall -Os -c giff.c -o giff.o || exit

# clang -march=native -std=c99 -Wall -Os -c msf_gif.c -o msf_gif.o || exit
# clang -march=native -std=c++11 -Wall -Os -c test.cpp -o test.o || exit
# clang msf_gif.o test.o -o giff -lstdc++ -lSDL2 || exit
# rm test.o msf_gif.o

clang -march=native -std=c99 -Wall -O0 -g -c msf_gif.c -o msf_gif.o || exit
clang -march=native -std=c++11 -Wall -O0 -g -c test.cpp -o test.o || exit
clang msf_gif.o test.o -o giff -lstdc++ -lSDL2 || exit

# clang -march=native -std=c99 -Wall -O0 -g -c msf_gif.c -o msf_gif.o -fsanitize=address || exit
# clang -march=native -std=c++11 -Wall -O0 -g -c test.cpp -o test.o -fsanitize=address || exit
# clang msf_gif.o test.o -o giff -lstdc++ -lSDL2 -fsanitize=address || exit

./giff
# clang -std=c++11 -Wall giff.c -E > out.cpp
