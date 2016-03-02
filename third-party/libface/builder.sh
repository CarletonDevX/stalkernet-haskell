source $stdenv/setup

cp -r $src orig
chmod -R +w orig
patch -s -p 0 < $patch

mkdir $out
cmake -DBUILD_EXAMPLES=1 -DCMAKE_INSTALL_PREFIX=$out orig
make
make install
