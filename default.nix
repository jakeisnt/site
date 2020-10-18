{ system ? builtins.currentSystem }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  site = import ./site.nix { inherit sources pkgs; };

  name = "jake/site";
  tag = "latest";

in pkgs.dockerTools.buildLayeredImage {
  inherit name tag;
  contents = [ site ];

  config = {
    Cmd = [ "/bin/site" ];
    Env = [ "ROCKET_PORT=5000" ];
    WorkingDir = "/";
  };
}
