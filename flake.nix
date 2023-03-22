{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = inputs: let
    lib = import "${inputs.nixpkgs}/lib";
    supportedSystems = lib.genAttrs [
      "x86_64-linux"
      "aarch64-linux"
      "x86_64-darwin"
    ];
    pkgs' = supportedSystems (system: import "${inputs.nixpkgs}" {
      inherit system;
      overlays = [
        (final: prev: with prev.haskell.lib; {
          haskellPackages = prev.haskellPackages.override {
            overrides = self: super: {
            rebuild = overrideCabal (super.callCabal2nix "rebuild" ./. { }) (drv: {
              postInstall = ''
                mkdir -p $out/share/{bash,zsh}-completion/completions
                $out/bin/rebuild --zsh-completion-script $out/bin/rebuild > $out/share/zsh-completion/completions/rebuild
                $out/bin/rebuild --bash-completion-script $out/bin/rebuild > $out/share/bash-completion/completions/rebuild
              '';
              librarySystemDepends = with super; [
                prev.nix
                prev.openssh
              ];
            });
          };
        };
        })
      ];
    });
  in rec {
    packages = supportedSystems (system: let
      pkgs = pkgs'.${system};
    in {
      rebuild = pkgs.haskellPackages.rebuild;
    });
    devShell = supportedSystems (system: pkgs'.${system}.haskellPackages.shellFor {
      packages = ps: with ps; [ rebuild ];
      nativeBuildInputs = with pkgs'.${system}; [
        cabal-install
      ];
      withHoogle = false;
    });
  };
}
