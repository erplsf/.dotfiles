{ inputs, lib, ... }:

{
  systemConfig = lib.nixosSystem {
    system = "x86_64-linux";

    modules = [
      ./system/configuration.nix
    ];

    specialArgs = { inherit inputs; }; # pass all inputs further down
  };
}
