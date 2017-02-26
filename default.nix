# vim: et:sw=2:sts=2

{ stdenv, callPackage, haskellPackages, harmonicsType ? "free" }:

let
  tcd       = callPackage ./libtcd.nix    {};
  harmonics = callPackage ./harmonics.nix { type = harmonicsType; };
in
  with haskellPackages;
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
