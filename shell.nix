{ pkgs ? import <nixpkgs> { } }:

let
  moz_overlay = import (builtins.fetchTarball
    "https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz");
  nixpkgs = import <nixpkgs> { overlays = [ moz_overlay ]; };
  orgparse = pkgs.python38Packages.buildPythonPackage rec {
    pname = "orgparse";
    version = "0.1.4";

    src = pkgs.python38Packages.fetchPypi {
      inherit pname version;
      sha256 = "05n0h136x8q1hrnw7bvi31bgblnn13gjgn4i63wjw6dixyi0khqn";
    };

    propagatedBuildInputs = with pkgs.python38Packages; [
      jinja2
      setuptools_scm
    ];

    doCheck = false;
  };

  ruststable = (nixpkgs.latest.rustChannels.stable.rust.override {
    extensions = [ "rust-src" "rust-analysis" ];
  });

  customPython = pkgs.python38.buildEnv.override { extraLibs = [ orgparse ]; };

  sources = import ./nix/sources.nix;
  rust = import ./nix/rust.nix { inherit sources; };
  pkgs = import sources.nixpkgs { };

in pkgs.mkShell {
  buildInputs = with nixpkgs; [
    customPython
    rust

    # keep this line if you use bash
    pkgs.bashInteractive
  ];
}
