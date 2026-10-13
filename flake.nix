{
  description = "Advent of Code in Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    # relude's hint file is not in its Hackage package; keep the tag equal to
    # the package set's relude version
    relude-src = {
      url = "github:kowainik/relude/v1.2.2.2";
      flake = false;
    };
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "aarch64-darwin"
        "x86_64-linux"
      ];
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem =
        { pkgs, ... }:
        {
          # basePackages defaults to pkgs.haskellPackages: nixpkgs' default
          # GHC, the only one Hydra builds a full binary cache for
          haskellProjects.default.devShell = {
            tools = hp: {
              # nixpkgs recommends the top-level cabal-install over the
              # package set's
              cabal-install = pkgs.cabal-install;
              inherit (hp) fourmolu weeder stan;
              haskell-language-server = null;
              ghcid = null;
            };
            hoogle = false;
            # not mkShellArgs.packages: shellFor overrides it with the
            # project's Haskell packages
            mkShellArgs = {
              nativeBuildInputs =
                (with pkgs; [
                  jq # gates on stan's JSON: stan fails only on Error severity
                  just
                  nixfmt # formats flake.nix in `just fmt`
                  time # child CPU time and max RSS, GNU on both platforms
                ])
                ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
                  pkgs.perf
                  pkgs.util-linux
                ];
              RELUDE_HLINT = "${inputs.relude-src}/.hlint.yaml";
            };
          };

          formatter = pkgs.nixfmt;
        };
    };
}
