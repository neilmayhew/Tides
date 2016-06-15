# vim: et:sw=2:sts=2

{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;
with pkgs;
with haskellPackages;

let
  tcd = pkgs.callPackage ./libtcd.nix {};
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
    description = "A program for exploring tidal prediction data";
    license = stdenv.lib.licenses.mit;
  }
