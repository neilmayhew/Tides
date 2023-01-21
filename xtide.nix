{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;
with pkgs;
with xorg;

let
  tcd = callPackage ./libtcd.nix {};
in
  stdenv.mkDerivation rec {
    pname = "xtide";
    version = "2.15.5";
    src = fetchurl {
      url = "https://flaterco.com/files/xtide/xtide-${version}.tar.xz";
      sha256 = "0r3i1mqz59awpflcdssa7zy9fjni2qapajmkvi29b3by2v7hsimr";
    };
    buildInputs = [ zlib libpng tcd libX11 libXext libXpm libXaw Xaw3d ];
    meta = {
      description = "Harmonic tide clock and tide predictor";
      license = lib.licenses.gpl3;
    };
  }
