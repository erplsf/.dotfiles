{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:rycee/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  description = "Config for my NixOS testing VM.";

  outputs = { self, nixpkgs, home-manager }: {
    nixosConfigurations = {
      qemu-vm = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./system/configuration.nix
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.junk = import ./home-manager/main.nix;
          }
        ];
      };
    };
  };
}
