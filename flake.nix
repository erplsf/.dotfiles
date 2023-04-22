{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    prismlauncher.url =
      "github:prismlauncher/prismlauncher/a5c8b166fe70c430548196f0883ca1aaff816c23";
    # url =
    #   "github:prismlauncher/prismlauncher/a5c8b166fe70c430548196f0883ca1aaff816c23"; # NOTE: it worked before, now it's broken
    emacs.url = "github:nix-community/emacs-overlay";
    eks-node-viewer.url = "github:erplsf/eks-node-viewer";
    aws-sso-cli.url = "github:erplsf/aws-sso-cli";
  };

  description = "Config for my NixOS testing VM.";

  outputs = inputs@{ self, nixpkgs, home-manager, ... }:
    let
      basePath = ./nixos;
      lib = nixpkgs.lib;
      mLib = import (basePath + /lib) { inherit inputs lib; };
    in { nixosConfigurations = mLib.systemConfigs (basePath + /hosts); };
}
