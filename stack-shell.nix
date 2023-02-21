{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.haskell.compiler.ghc925 }:

with pkgs;

let
  tcd       = callPackage ./libtcd.nix    {};
  harmonics = callPackage ./harmonics.nix { type = "non-free"; };
  xtide     = callPackage ./xtide.nix     {};
in

haskell.lib.buildStackProject {
  inherit ghc;
  name = "tides-env";
  buildInputs = [
    tcd
    harmonics
    xtide
    tzdata
    zlib
  ];
  shellHook = ''
    export HFILE_PATH=${harmonics}/share/xtide
    export PATH=${xtide}/bin:$PATH
    export TZDIR=${pkgs.tzdata}/share/zoneinfo
  '';
}
