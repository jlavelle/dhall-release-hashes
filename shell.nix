let sources = import ./nix/sources.nix;
    pkgs = import sources.nixpkgs {};
    github-utils = import ./default.nix;
    hls = import sources.all-hls { inherit pkgs; version = "0.9.0"; ghc = "8.8.4"; };
in
github-utils.shellFor {
  withHoogle = false;
  tools = {
    cabal = "3.2.0.0";
    hlint = "3.2.1";
  };
  buildInputs = with pkgs.haskellPackages;
    [ ghcid
      hpack
      stylish-haskell
      hls
    ];
  exactDeps = true;
}
