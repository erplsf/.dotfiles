{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  description = "Config for my NixOS testing VM.";

  outputs = inputs @ { self, nixpkgs }:
    let
      basePath = ./nixos;
      lib = nixpkgs.lib;
      mLib = import (basePath + /lib) { inherit inputs lib; };
    in {
      nixosConfigurations = mLib.systemConfigs (basePath + /hosts);
    };
}
