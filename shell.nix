{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", harmonicsType ? "free" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage ./default.nix { inherit harmonicsType; };

in

  if pkgs.lib.inNixShell then drv.env else pkgs.haskell.lib.justStaticExecutables drv
