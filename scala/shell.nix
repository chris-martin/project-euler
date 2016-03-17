{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation {
  name = "euler";
  buildInputs = [pkgs.sbt];
};
