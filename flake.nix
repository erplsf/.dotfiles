{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    prismlauncher = {
      url = "github:prismlauncher/prismlauncher";
      rev = "a5c8b166fe70c430548196f0883ca1aaff816c23";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    eks-node-viewer = {
      url = "github:erplsf/eks-node-viewer";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    aws-sso-cli = {
      url = "github:erplsf/aws-sso-cli";
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
