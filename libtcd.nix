{ stdenv, lib, fetchurl }:

stdenv.mkDerivation rec {
  pname = "libtcd";
  version = "2.2.7-r3";
  src = fetchurl {
    url = "https://flaterco.com/files/xtide/libtcd-${version}.tar.xz";
    sha256 = "0xyqlal5f0625dw743hak406h76ng55rpabnqrbsn7kpzfmfkpg1";
  };
  meta = {
    description = "A software API for reading and writing Tide Constituent Database (TCD) files";
    license = lib.licenses.gpl3;
  };
}
