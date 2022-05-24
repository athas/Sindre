let
  pkgs = import <nixpkgs> {};
in
pkgs.stdenv.mkDerivation {
  name = "sindre";
  buildInputs = [
    pkgs.pkgconfig
    pkgs.pcre
    pkgs.xorg.libXft
    pkgs.xorg.libX11
    pkgs.xorg.libXext
    pkgs.xorg.libXrandr
    pkgs.xorg.libXScrnSaver
  ];
}
