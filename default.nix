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
    (pkgs.texlive.combine {
      inherit (pkgs.texlive) scheme-minimal latexmk newunicodechar babel import hyperref biblatex caption adjustbox makecell siunitx attachfile2 minted letltxmacro xkeyval collectbox pgf fvextra upquote lineno fancyvrb catchfile xstring framed float collection-latex;
    })
  ];
}
