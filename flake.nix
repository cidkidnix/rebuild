{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    mars = {
      url = "github:reflex-frp/reflex-platform/mars_obelisk";
      flake = false;
    };
  };

  outputs = inputs: let
    lib = import "${inputs.nixpkgs}/lib";
    supportedSystems = lib.genAttrs [
      "x86_64-linux"
      "aarch64-linux"
      "x86_64-darwin"
    ];
  in rec {
    packages = supportedSystems (system: let
      marsProject = import inputs.mars {
        inherit system;
      };
      proj = marsProject ({ pkgs, ... }: {
        name = "earth";
        src = ./.;
        compiler-nix-name = "ghc926";
        overrides = [
          ({ config, lib, pkgs, ... }: {
            packages.earth.components.library.build-tools = [
              pkgs.nix
              pkgs.openssh
            ];
          })
        ];
        shellTools = {
          cabal = "3.6.2.0";
          haskell-language-server = "1.9.1.0";
          ghcid = "0.8.8";
        };
        shells = ps: with ps; [
          earth
        ];
      });
    in {
      earth = proj.hsPkgs.earth.components.exes.earth;
      inherit proj;
    });

    devShell = supportedSystems (system: packages.${system}.proj.shells.ghc);
  };
}
