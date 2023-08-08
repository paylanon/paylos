####
#### ╔══════╦╦═════╗
#### ║╔═╦═╦╦╣╠═╦══╗║
#### ║║║╠╝║║║║║╠╗╚╣║
#### ║║╔╩═╬╗╠╩═╩══╝║
#### ╚╩╩══╩═╩══════╝
####
## NIXOS FLAKE

{
  description = "A Dystopian NixOS/Hyprland Configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable"
  }

  outputs = { self, nixpkgs }: {

    system = "x86_64-linux";
    modules = [ ./configuration.nix ];

  };
}
