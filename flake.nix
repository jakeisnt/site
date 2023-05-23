{
  description = "jake website";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";

    # Used for shell.nix
    flake-compat = {
      url = github:edolstra/flake-compat;
      flake = false;
    };
  };

  outputs = { self, nixpkgs, utils, ... } @ inputs:
    utils.lib.eachDefaultSystem (system:
      let
        inherit (lib) attrValues;
        pkgs = import nixpkgs { inherit system; };
        lib = pkgs.lib;

        deploy = pkgs.writeScriptBin "deploy" ''
          #!/usr/bin/env sh
          clj -X deploy/-main "$@"
        '';

        build = pkgs.writeScriptBin "build" ''
          #!/usr/bin/env sh
          clj -X main/-main "$@"
        '';

        serve = pkgs.writeScriptBin "serve" ''
          #!/usr/bin/env sh
          clj -X serve/-main "$@"
        '';
      in rec {
        devShell = with pkgs; mkShell {
          name = "site";
          buildInputs = [
            inotify-tools

            clojure
            leiningen
            clojure-lsp
            sass

            serve
            build
            deploy
          ];

          LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath(with pkgs; [openssl sqlite])}:LD_LIBRARY_PATH";
        };
      });
}
