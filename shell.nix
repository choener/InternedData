{ pkgs ? <nixpkgs> }:

with import pkgs {};

let
  hsPkgs0 = haskellPackages.override {
    overrides = hself: hsuper:
      {
      };
  }; # haskellPackages override
  hsPkgs = hsPkgs0.extend (haskell.lib.packageSourceOverrides {
        InternedData = ./.;
  }); # extend
  # my own little tool
  cabalghcisrc =
    let local = ~/Documents/University/active/ghcicabal;
    in  if builtins.pathExists local
        then local
        else builtins.fetchGit {
          url = https://github.com/choener/ghcicabal;
          ref = "master";
        };
  cabalghci = hsPkgs.callPackage cabalghcisrc {};
in

hsPkgs.shellFor {
  packages = p: [
    p.InternedData
                ];
  withHoogle = true;
  buildInputs = [
    cabal-install
    llvm
    cabalghci
  ];
} # shellFor

