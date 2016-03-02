{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, ansi-wl-pprint, async, attoparsec
      , base, bytestring, cereal, cereal-text, html-conduit, lens, stdenv
      , stm, text, wreq, xml-conduit, xml-lens
      }:
      mkDerivation {
        pname = "stalkernet";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson ansi-wl-pprint async attoparsec base bytestring cereal
          cereal-text html-conduit lens stm text wreq xml-conduit xml-lens
        ];
        executableHaskellDepends = [
          aeson ansi-wl-pprint async base bytestring cereal stm
        ];
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
