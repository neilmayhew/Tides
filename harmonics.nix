{ stdenv, fetchurl, type ? "free" }:

let
  free = {
    url = ftp://ftp.flaterco.com/xtide/harmonics-dwf-20151227-free.tar.bz2;
    sha256 = "1b57smza69kqv9wmjd4ycgisnafrryzgsgpz6ij6zzffjkysr0mk";
    version = "2015.12.27";
    sourceRoot = "";
  };
  nonfree = {
    url = ftp://ftp.flaterco.com/xtide/harmonics-dwf-20111230-nonfree.tar.bz2;
    sha256 = "0759dk9rdr5rklz3xg2riy02afj7s239w61irmjgvhd4psnx4k6l";
    version = "2011.12.30";
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
