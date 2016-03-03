{ stdenv, fetchgit, cmake, opencv, qt5 }:

stdenv.mkDerivation rec {

  version = "0.5";
  name = "openbr-${version}";

  # Tag v1.1.0
  src = fetchgit {
    url = "https://github.com/biometrics/openbr";
    rev = "e6fbbaa60f74fb29bdb432f27f1683526cb84069";
    sha256 = "ebc74aa030b55bf6c98968b78f1a4066a1678fed55d1cf0e79aab8572c19f213";
  };

  buildInputs = [ opencv qt5.base qt5.svg ];

  nativeBuildInputs = [ cmake ];

  enableParallelBuilding = true;

  cmakeFlags = [
    "-DCMAKE_BUILD_TYPE=Release"
  ];

  meta = {
    description = "Open Source Biometric Recognition";
    homepage = http://openbiometrics.org/;
    license = stdenv.lib.licenses.asl20;
    maintainers = with stdenv.lib.maintainers; [flosse];
    platforms = with stdenv.lib.platforms; linux;
  };
}
