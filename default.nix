# vim: et:sw=2:sts=2

{ stdenv, callPackage, haskellPackages, tzdata, harmonicsType ? "free" }:

let
  tcd       = callPackage ./libtcd.nix    {};
  harmonics = callPackage ./harmonics.nix { type = harmonicsType; };
  xtide     = callPackage ./xtide.nix     {};

  inherit (stdenv.lib) concatStringsSep;
in
  with haskellPackages;

  mkDerivation rec {
    pname = "Tides";
    version = "0.1.0.0";
    src = ./.;
    isLibrary = false;
    isExecutable = true;
    libraryHaskellDepends = [ base ];
    executableHaskellDepends = [ base HSH random time time-locale-compat tz QuickCheck ];
    executableSystemDepends = [ tcd ];
    testHaskellDepends = [ base process ];
    enableSharedExecutables = false;
    enableSharedLibraries = false;
    configureFlags = [
      "--ghc-option=-DDEFAULT_TIDE_DB_PATH=\"${harmonics}/share/xtide\""
    ];
    shellHook = ''
      configure() {
        cabal configure --enable-tests '${concatStringsSep "' '" configureFlags}' "$@"
      }
      export HFILE_PATH=${harmonics}/share/xtide
      export PATH=${xtide}/bin:$PATH
    '';
    preCheck = "export TZDIR=${tzdata}/share/zoneinfo";
    doCheck = (harmonicsType != "free");
    description = "A program for exploring tidal prediction data";
    license = stdenv.lib.licenses.mit;
  }
