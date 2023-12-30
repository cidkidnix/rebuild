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
    pkgs'' = inputs.nixpkgs.legacyPackages.x86_64-linux;
    supportedSystems = lib.genAttrs [
      "x86_64-linux"
      "aarch64-linux"
      "x86_64-darwin"
    ];
  in rec {
    hydraJobs = rec {
      recurseForDerivations = true;
      build = supportedSystems (system: packages."${system}".earth);
      projects = supportedSystems (system: packages."${system}".proj);
      devShells = pkgs''.releaseTools.aggregate
        { name = "earth-devshells";
          constituents = [ projects.x86_64-linux.shells.ghc ];
          meta.description = "Earth DevShells";
        };
      release = pkgs''.releaseTools.aggregate
        { name = "earth";
          constituents = [ build.x86_64-linux ];
          meta.description = "Earth build";
        };
    };
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
