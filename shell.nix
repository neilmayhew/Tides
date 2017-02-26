{ nixpkgs ? import <nixpkgs> {}, harmonicsType ? "free" }:

with nixpkgs;
with pkgs;

let
	drv = callPackage ./default.nix { inherit harmonicsType; };
in
	if pkgs.lib.inNixShell then drv.env else drv
