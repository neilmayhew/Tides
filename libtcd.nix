{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;
with pkgs;

stdenv.mkDerivation {
  name = "libtcd";
  version = "2.2.7-r2";
  src = fetchurl {
  	url = ftp://ftp.flaterco.com/xtide/libtcd-2.2.7-r2.tar.bz2;
	sha256 = "150a099n4bhfrk6n2mnv2ai1x3l23k8jr4cls1rca1j1p0cg5wdg";
  };
  meta = {
  	description = "A software API for reading and writing Tide Constituent Database (TCD) files";
  	license = stdenv.lib.licenses.gpl3;
  };
}
