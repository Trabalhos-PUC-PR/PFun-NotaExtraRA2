{ pkgs }: {
    deps = [
      pkgs.python310
        pkgs.haskellPackages.ghc
        pkgs.haskell-language-server
    ];
}