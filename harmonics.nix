{ stdenv, lib, fetchurl, type ? "free" }:

let
  free = rec {
    version = "20190620";
    src = fetchurl {
      url = "https://flaterco.com/files/xtide/harmonics-dwf-${version}-free.tar.bz2";
      sha256 = "05pn97ls3igq3jjyy7kjcwzh937kl47sbk1b1xhjkhb10zrld5c7";
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
