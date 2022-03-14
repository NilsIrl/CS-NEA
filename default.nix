{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = [
  ] ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
    pkgs.libiconv
  ];

  nativeBuildInputs = [
    pkgs.python39Packages.pygments
    pkgs.biber
    pkgs.wasm-pack
    pkgs.tectonic
    pkgs.texlive.combined.scheme-full
  ];
}
