source $stdenv/setup
mkdir -p $out/bin
g++ -lopencv_contrib -o $out/bin/facerec $src/facerec.cpp
