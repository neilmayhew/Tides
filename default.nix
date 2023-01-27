# vim: et:sw=2:sts=2

{ stdenv, lib, callPackage, mkDerivation, harmonicsType ? "free"
, base, base-compat, process, random, time, time-locale-compat, tz, QuickCheck
, pkgs
}:

let
  tcd       = callPackage ./libtcd.nix    {};
  harmonics = callPackage ./harmonics.nix { type = harmonicsType; };
  xtide     = callPackage ./xtide.nix     {};

  inherit (lib) concatStringsSep;
in
  mkDerivation rec {
    pname = "Tides";
    version = "0.1.0.0";
    src = ./.;
    isLibrary = false;
    isExecutable = true;
    libraryHaskellDepends = [ base ];
    executableHaskellDepends = [ base base-compat process random time time-locale-compat tz QuickCheck ];
    executableSystemDepends = [ tcd ];
    testHaskellDepends = [ base process ];
    enableSharedExecutables = false;
    enableSharedLibraries = false;
    shellHook = ''
      export HFILE_PATH=${harmonics}/share/xtide
      export PATH=${xtide}/bin:$PATH
      export TZDIR=${pkgs.tzdata}/share/zoneinfo
    '';
    preCheck = shellHook;
    doCheck = (harmonicsType != "free");
    description = "A program for exploring tidal prediction data";
    license = lib.licenses.mit;
  }
