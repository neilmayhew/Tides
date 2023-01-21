{ stdenv, lib, fetchurl, type ? "free" }:

let
  free = rec {
    version = "20191229";
    src = fetchurl {
      url = "https://flaterco.com/files/xtide/harmonics-dwf-${version}-free.tar.xz";
      sha256 = "0dfd4cj10k6jh35anps65gzznq1r4qi1jjfdg5dvwyxla6m9hizi";
    };
    sourceRoot = "";
  };
  nonfree = rec {
    version = "20111230";
    src = Downloads/ftp.flaterco.com/xtide/harmonics-dwf-${version}-nonfree.tar.bz2;
    sourceRoot = ".";
  };
  origin = if type == "free" then free else nonfree;
in
  stdenv.mkDerivation {
    pname = "xtide-data-${type}";
    inherit (origin) version src sourceRoot;
    installPhase = ''
      shopt -s extglob
      mkdir -p $out/share/xtide/
      cp -a *.tcd $out/share/xtide/
      mkdir -p $out/share/doc/
      cp -a !(*.tcd) $out/share/doc/
      rm -f $out/share/doc/env-vars
    '';
    meta = {
      description = "Tidal harmonics database for libtcd (${type})";
      license = with lib.licenses; if type == "free" then publicDomain else unfree;
      maintainer = with lib.maintainers; [ neilmayhew ];
    };
  }
