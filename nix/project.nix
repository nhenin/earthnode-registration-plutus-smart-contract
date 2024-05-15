{ repoRoot, inputs, pkgs, lib, system }:

let

  cabalProject' = pkgs.haskell-nix.cabalProject' ({ config, pkgs, ... }:
    let
      # When `isCross` is `true`, it means that we are cross-compiling the project.
      # WARNING You must use the `pkgs` coming from cabalProject' for `isCross` to work.
      isCross = pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform;
    in
    {
      name = "en-registration";
      src = ../.;
      compiler-nix-name = lib.mkDefault "ghc96";
      shell.withHoogle = false;
      inputMap = {
        "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.iogx.inputs.CHaP;
      };
      modules = [{
        packages = {
          en-registration.ghcOptions = [ "-Werror" ];
        };
      }];
      flake.variants.traced = {
        modules = [{
          packages = {
            en-registration = {
              ghcOptions = [ "-Werror" ];
              configureFlags = [ "--flag=+trace-plutus" ];
            };
          };
        }];
      };
    });

  cabalProject = cabalProject'.appendOverlays [ ];

  project = lib.iogx.mkHaskellProject {
    inherit cabalProject;
    shellArgs = repoRoot.nix.shell;
  };

in

project
