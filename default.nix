# vim: et:sw=2:sts=2

{ nixpkgs ? import <nixpkgs> {}, harmonicsType ? "free" }:

with nixpkgs;
with pkgs;
with haskellPackages;

let
  tcd       = pkgs.callPackage ./libtcd.nix    {};
  harmonics = pkgs.callPackage ./harmonics.nix { type = harmonicsType; };
in
  mkDerivation {
    pname = "Tides";
    version = "0.1.0.0";
    src = ./.;
    isLibrary = true;
    isExecutable = true;
    libraryHaskellDepends = [ base ];
    executableHaskellDepends = [ base HSH old-locale random time tz ];
    executableSystemDepends = [ tcd ];
    testHaskellDepends = [ base process ];
    configureFlags = [ "--ghc-option=-DDEFAULT_TIDE_DB_PATH=\"${harmonics}/share/xtide\"" ];
    description = "A program for exploring tidal prediction data";
    license = stdenv.lib.licenses.mit;
  }
