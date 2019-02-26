{pkgs ? import ./nix/pkgs.nix {}}:
pkgs.haskellPackages.callPackage ./adrenaline.nix {}
