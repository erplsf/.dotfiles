{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    polymc = {
      url = "github:PolyMC/PolyMC";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  description = "Config for my NixOS testing VM.";

  outputs = inputs@{ self, nixpkgs, home-manager, ... }:
    let
      basePath = ./nixos;
      lib = nixpkgs.lib;
      mLib = import (basePath + /lib) { inherit inputs lib; };
    in { nixosConfigurations = mLib.systemConfigs (basePath + /hosts); };
}
