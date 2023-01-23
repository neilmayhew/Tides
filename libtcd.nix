{ stdenv, lib, fetchurl }:

stdenv.mkDerivation rec {
  pname = "libtcd";
  version = "2.2.7-r2";
  src = fetchurl {
    url = "https://flaterco.com/files/xtide/libtcd-${version}.tar.bz2";
    sha256 = "150a099n4bhfrk6n2mnv2ai1x3l23k8jr4cls1rca1j1p0cg5wdg";
  };
  meta = {
    description = "A software API for reading and writing Tide Constituent Database (TCD) files";
    license = lib.licenses.gpl3;
  };
}
