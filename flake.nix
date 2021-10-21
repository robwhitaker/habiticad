{
  description = "Daemon that listens for dbus events and launches Habitica requests";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        haskellPackages = pkgs.haskell.packages.ghc8107.override {
          overrides = final: prev:
            let
              hsOverride = pkg: final.callPackage (./nix-overrides + "/${pkg}.nix") {};
            in {
              habiticad = final.callPackage ./. {};
            };
        };
        projectGhc = haskellPackages.ghcWithHoogle (_:
          haskellPackages.habiticad.getBuildInputs.haskellBuildInputs
        );
      in
      rec {
        packages = flake-utils.lib.flattenTree {
          habiticad = haskellPackages.habiticad;
        };
        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            projectGhc

            cabal2nix
            cabal-install

            haskell-language-server
          ];
        };
        defaultPackage = packages.habiticad;
      }
    );
}
