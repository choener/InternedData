with (import <nixpkgs> {});
with haskell.lib;

rec {
  hsSrcSet
    =  (import ../Lib-bimaps).hsSrcSet
    // {InternedData = ./.;};
  hsPkgs = haskellPackages.extend (packageSourceOverrides hsSrcSet);
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.InternedData ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc
      DPutils
      OrderedBits
    ];
  };
}
