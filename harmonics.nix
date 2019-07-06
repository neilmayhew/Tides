{ stdenv, fetchurl, type ? "free" }:

let
  free = rec {
    version = "20190620";
    url = "https://flaterco.com/files/xtide/harmonics-dwf-${version}-free.tar.bz2";
    sha256 = "05pn97ls3igq3jjyy7kjcwzh937kl47sbk1b1xhjkhb10zrld5c7";
    sourceRoot = "";
  };
  nonfree = rec {
    version = "20111230";
    url = "ftp://ftp.flaterco.com/xtide/harmonics-dwf-${version}-nonfree.tar.bz2";
    sha256 = "0759dk9rdr5rklz3xg2riy02afj7s239w61irmjgvhd4psnx4k6l";
    sourceRoot = ".";
  };
  origin = if type == "free" then free else nonfree;
in
  stdenv.mkDerivation {
    name = "xtide-data-${type}";
    description = "Tidal harmonics database for libtcd (${type})";
    #license = stdenv.lib.licenses.gpl3;
    inherit (origin) version sourceRoot;
    src = fetchurl { inherit (origin) url sha256; };
    installPhase = ''
      shopt -s extglob
      mkdir -p $out/share/xtide/
      cp -a *.tcd $out/share/xtide/
      mkdir -p $out/share/doc/
      cp -a !(*.tcd) $out/share/doc/
      rm -f $out/share/doc/env-vars
    '';
  }
