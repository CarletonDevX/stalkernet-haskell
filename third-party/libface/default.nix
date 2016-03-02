{ stdenv, fetchsvn, which, cmake, opencv, qt4 }:

stdenv.mkDerivation {
  name = "libface-0.1";
  src = fetchsvn {
    url = "https://svn.code.sf.net/p/libface/code/tags/0.1";
    rev = 330;
    sha256 = "1n5arf54dn1mf48mvd92mdv8kp3n1gv111zxyny1lj7p9jlrbz5g";
  };
  buildInputs = [ cmake opencv qt4 ];
  builder = ./builder.sh;
  patch = ./cmake.patch;
}

