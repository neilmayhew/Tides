{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;
with pkgs;
with xorg;

let
  tcd = callPackage ./libtcd.nix {};
in
  stdenv.mkDerivation rec {
    pname = "xtide";
    version = "2.15.6";
    src = fetchurl {
      url = "https://flaterco.com/files/xtide/xtide-${version}.tar.xz";
      sha256 = "1kqcs30k4mcr984ryac2b8ripfv58z1m5na3ivrx8xx2l4gyrpwi";
    };
    buildInputs = [ zlib libpng tcd libX11 libXext libXpm libXaw Xaw3d ];
    meta = {
      description = "Harmonic tide clock and tide predictor";
      license = lib.licenses.gpl3;
    };
  }
