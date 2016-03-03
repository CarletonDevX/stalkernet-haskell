with import <nixpkgs> {}; {
  libface = callPackage ./libface {};
  openbr = callPackage ./openbr {};
}
