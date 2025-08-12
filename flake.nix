{
  description = "priime0 nixos configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, home-manager, flake-utils, ... }@inputs:
    let inherit (self) outputs;
    in {
      nixosConfigurations.framework = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inputs = inputs; };
        modules = [
          home-manager.nixosModules.default
          ./nix/hosts/common/sys.nix
          ./nix/hosts/framework/sys.nix
        ];
      };

      nixosConfigurations.chaewon = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inputs = inputs; };
        modules = [
          home-manager.nixosModules.default
          ./nix/hosts/common/sys.nix
          ./nix/hosts/chaewon/sys.nix
        ];
      };
    };
}
