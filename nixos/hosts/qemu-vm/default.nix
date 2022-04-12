{ inputs, lib, ... }:

{
  systemConfig = lib.nixosSystem {
    system = "x86_64-linux";

    modules = [
      ./system/configuration.nix
      inputs.home-manager.nixosModules.home-manager { # TODO: Refactor out to a file
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.users.junk = import ./home-manager/junk.nix;
      }
    ];

    specialArgs = { inherit inputs; }; # pass all inputs further down
  };
}
