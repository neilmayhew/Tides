{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;
with pkgs;
with xorg;

let
  tcd = callPackage ./libtcd.nix {};
in
  stdenv.mkDerivation {
    name = "xtide";
    version = "2.15.1";
    src = fetchurl {
      url = ftp://ftp.flaterco.com/xtide/xtide-2.15.1.tar.bz2;
      sha256 = "10f67di1vmvs0j0gfchrpg0x2pl4hk5z4lz8jvidv7r62yxszi75";
    };
    buildInputs = [ zlib libpng tcd xlibsWrapper libXpm libXaw Xaw3d ];
    meta = {
      description = "Harmonic tide clock and tide predictor";
      license = stdenv.lib.licenses.gpl3;
    };
  }
