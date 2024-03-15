# vim: et:sw=2:sts=2

{ stdenv, lib, callPackage, mkDerivation, harmonicsType ? "free"
, base, base-compat, containers, directory, filepath, hspec, hspec-golden
, infinite-list, mtl, process, QuickCheck, random, time, time-locale-compat, tz
, pkgs
}:

let
  tcd       = callPackage ./libtcd.nix    {};
  harmonics = callPackage ./harmonics.nix { type = harmonicsType; };
  xtide     = callPackage ./xtide.nix     {};
in
  mkDerivation rec {
    pname = "Tides";
    version = "0.1.0.0";
    src = lib.cleanSource ./.;
    isLibrary = true;
    isExecutable = true;
    libraryHaskellDepends = [ base directory filepath ];
    librarySystemDepends = [ tcd ];
    executableHaskellDepends = [
      base containers directory filepath infinite-list mtl process QuickCheck
      random time time-locale-compat tz ];
    executableSystemDepends = [ tcd ];
    testHaskellDepends = [
      base base-compat directory filepath hspec hspec-golden mtl process
      random time time-locale-compat tz
    ];
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
