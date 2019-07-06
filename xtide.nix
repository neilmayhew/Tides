{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;
with pkgs;
with xorg;

let
  tcd = callPackage ./libtcd.nix {};
in
  stdenv.mkDerivation rec {
    name = "xtide";
    version = "2.15.2";
    src = fetchurl {
      url = "https://flaterco.com/files/xtide/xtide-${version}.tar.bz2";
      sha256 = "0cb9lhzs2asqlp0m505qgzlykwjlcq2rf81kk3kvcmdns67jw768";
    };
    buildInputs = [ zlib libpng tcd xlibsWrapper libXpm libXaw Xaw3d ];
    meta = {
      description = "Harmonic tide clock and tide predictor";
      license = stdenv.lib.licenses.gpl3;
    };
  }
