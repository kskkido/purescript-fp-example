{
  description = "Fullstack conduit app";
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
    easy-ps = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
  };
  outputs = { self, nixpkgs, flake-utils, easy-ps, ... }:
    flake-utils.lib.eachSystem [ "x86_64-darwin" ] (system:
      let
        ghc = "ghc925";
        overlay = self: super: {
          haskell = super.haskell // {
            packages = super.haskell.packages // {
              ${ghc} = super.haskell.packages.${ghc}.extend (self: super: {
                honduit-core = self.callCabal2nix "honduit-core" ./packages/honduit-core {};
                honduit-database = self.callCabal2nix "honduit-database" ./packages/honduit-database {};
                honduit-web-api = self.callCabal2nix "honduit-web-api" ./packages/honduit-web-api {};
              });
            };
          };
          purescriptPackages = import easy-ps {
            pkgs = self;
          };
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            overlay
          ];
        };
      in
        with pkgs; rec {
          packages = {
            honduit-core = haskell.packages.${ghc}.honduit-core;
            honduit-database = haskell.packages.${ghc}.honduit-database;
            honduit-web-api = haskell.packages.${ghc}.honduit-web-api;
            default = haskell.packages.${ghc}.honduit-web-api;
          };
          devShell = mkShell {
            buildInputs = [
              docker
              nodejs-18_x
              postgresql_14
              purescriptPackages.purs-0_14_3
              purescriptPackages.spago
              packages.honduit-core
              packages.honduit-database
              packages.honduit-web-api
            ];
          shellHook =
            ''
              echo "Hello shell"
              BUILD_ENV=$(cat configs/local/.env)
              export $(echo $BUILD_ENV | xargs)
            '';
          };
        }
    );
}

