![paylos-header2](https://github.com/paylhorse/paylos/assets/74363924/bf211d6e-f5fe-40ce-8de9-d1eb9cfa3e91)

**A NixOS/Sway Configuration that happens to be a dystopia, as well as some dotfiles.**

[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

Includes the Neovim Configuration: [**DEVILVIM**](https://github.com/paylhorse/paylos/tree/main/dotfiles/nvim)

Art: Simon Stålenhag, sue me

## Usage

**❅ Cherry-pick anything that takes your fancy from the dotfiles directory!**

Generally, these files goes to your ***/home/username/.config*** directory in folders of the same name, for Linux users.

**❅ Alternatively, if you're running NixOS, you may choose to be adventerous and adopt paylos as your desktop config.**

Paylos makes for a great boilerplate: it comes with all the essentials of a good Linux desktop, with the option to activate a slew of little quirks catered to powerusers. Guide below.

## About NixOS and Paylos

For those unaware, NixOS is a Linux distribution in which the system in it's entirety is declared in what's essentially a comprehensive dotfile.

## NixOS Installation
(1) Clone and navigate into this repository:
```console
[user@nixos:~]$ git clone https://github.com/paylhorse/paylos.git
[user@nixos:~]$ cd paylos
```
(2) Build paylos provisionally as a flake, and take it for a spin:
```console
[user@nixos:~/paylos]$ sudo nixos-rebuild switch --flake .#paylos
```
Here is a good breakpoint to ensure that everything's to your liking, making edits to the config files and rebuilding with the command above as necessary.

(3) Once you're ready to adopt paylos as your default, copy the contents of the repository into your NixOS config directory.

Generally, this would be ***/etc/nixos***.

Otherwise, it would be wherever you have your *configuration.nix*, which the *configuration.nix* of this repository would replace.
```console
[user@nixos:~/paylos]$ sudo cp . /etc/nixos/
[user@nixos:~/paylos]$ sudo nixos-rebuild switch
```
**All done!** Welcome to your slick new desktop.
