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
        (self: super: with super.haskell.lib; {
          haskellPackages = super.haskellPackages // {
            rebuild = overrideCabal (super.haskellPackages.callCabal2nix "rebuild" ./. { }) (drv: {
              postInstall = ''
                mkdir -p $out/share/{bash,zsh}-completion/completions
                $out/bin/rebuild --zsh-completion-script $out/bin/rebuild > $out/share/zsh-completion/completions/rebuild
                $out/bin/rebuild --bash-completion-script $out/bin/rebuild > $out/share/bash-completion/completions/rebuild
              '';
              librarySystemDepends = with super; [
                nix
                openssh
              ];
            });
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
      buildInputs = p: with p; [
        Cabal
      ];
      withHoogle = false;
    });
  };
}
