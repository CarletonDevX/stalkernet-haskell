{ stdenv, opencv }:

stdenv.mkDerivation {
  name = "facerec";
  src = ./.;
  builder = ./builder.sh;
  buildInputs = [ opencv ];
}
