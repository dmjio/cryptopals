{ pkgs ? import <nixpkgs> {} }:
  pkgs.haskellPackages.ghcWithPackages (p: with p; [ bytestring split ])

