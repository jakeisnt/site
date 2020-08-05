{ pkgs ? import <nixpkgs> { } }:

# { pkgs ? import (fetchTarball "https://git.io/Jf0cc") { } }:

let
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

  customPython = pkgs.python38.buildEnv.override { extraLibs = [ orgparse ]; };

in pkgs.mkShell {
  buildInputs = [
    customPython

    # keep this line if you use bash
    pkgs.bashInteractive
  ];
}
