{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  description = "Config for my NixOS testing VM.";

  outputs = inputs @ { self, nixpkgs }: {
    nixosConfigurations = {
      qemu-vm = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";

        modules = [
          ./system/configuration.nix
        ];

        specialArgs = { inherit inputs; }; # pass all inputs further down
      };
    };
  };
}
