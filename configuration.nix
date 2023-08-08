####
#### ╔══════╦╦═════╗
#### ║╔═╦═╦╦╣╠═╦══╗║
#### ║║║╠╝║║║║║╠╗╚╣║
#### ║║╔╩═╬╗╠╩═╩══╝║
#### ╚╩╩══╩═╩══════╝
####
## NIXOS CONFIGURATION

{ config, pkgs, ... }:

{
  imports =
      [ # Include the results of the hardware scan.
        ./hardware-configuration.nix
        <home-manager/nixos>
      ];

    # Bootloader.
    boot.loader.efi.efiSysMountPoint = "/boot";

    # Use GRUB instead of system-md
    boot.loader.grub.enable = true;
    boot.loader.grub.efiInstallAsRemovable = true;
    boot.loader.grub.efiSupport = true;
    boot.loader.grub.device = "nodev";
    boot.loader.grub.useOSProber = true;

    networking.hostName = "nixos"; # Define your hostname.
    # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

    # Configure network proxy if necessary
    # networking.proxy.default = "http://user:password@proxy:port/";
    # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

    # Enable networking
    networking.networkmanager.enable = true;

    # Set your time zone.
    time.timeZone = "America/Los_Angeles";

    # Select internationalisation properties.
    i18n.defaultLocale = "en_US.UTF-8";

    i18n.extraLocaleSettings = {
      LC_ADDRESS = "en_US.UTF-8";
      LC_IDENTIFICATION = "en_US.UTF-8";
      LC_MEASUREMENT = "en_US.UTF-8";
      LC_MONETARY = "en_US.UTF-8";
      LC_NAME = "en_US.UTF-8";
      LC_NUMERIC = "en_US.UTF-8";
      LC_PAPER = "en_US.UTF-8";
      LC_TELEPHONE = "en_US.UTF-8";
      LC_TIME = "en_US.UTF-8";
    };
    
    nix.settings.experimental-features = ["nix-command" "flakes"];
    
    xdg.portal.enable = true;
    xdg.portal.extraPortals = [pkgs.xdg-desktop-portal-hyprland pkgs.xdg-desktop-portal-gtk];
    
    programs.hyprland.enable = true;
    environment.sessionVariables.NIXOS_OZONE_WL = "1";

    # Enable the X11 windowing system.
    #services.xserver.enable = true;

    # Enable the GNOME Desktop Environment.
    services.xserver.displayManager.gdm.enable = true;
    #services.xserver.desktopManager.gnome.enable = true;

    # Configure keymap in X11
    #services.xserver = {
    #  layout = "us";
    #  xkbVariant = "";
    #};

    # Enable CUPS to print documents.
    services.printing.enable = true;

    # Enable sound with pipewire.
    sound.enable = true;
    hardware.pulseaudio.enable = false;
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      # If you want to use JACK applications, uncomment this
      #jack.enable = true;

      # use the example session manager (no others are packaged yet so this is enabled by default,
      # no need to redefine it in your config for now)
      #media-session.enable = true;
    };

    # Enable touchpad support (enabled default in most desktopManager).
    # services.xserver.libinput.enable = true;

    # Allow unfree packages
    nixpkgs.config.allowUnfree = true;

    # List packages installed in system profile. To search, run:
    # $ nix search wget
    environment.systemPackages = with pkgs; [
    ##  PACKAGES TO BE INSTALLED GLOBALLY LISTED HERE

    # Essentials
    rofi-wayland
    pkgs.waybar
    pkgs.mako
    libnotify
    cinnamon.nemo
    swww
    swaylock-fancy
    swayidle
    pamixer
    light
    brillo
    pkgs.networkmanagerapplet
    vscode
    neovim
    rustup
    gcc

    #  Terminal
    wezterm
    # kitty
    neofetch
    pfetch
    
    #  Misc. Tools
    unzip
    grim
    slurp
    gnome.gnome-tweaks
    gnomeExtensions.system-monitor
    gparted
    
    nix-prefetch
    nix-prefetch-git
    gnome.gnome-font-viewer

    #  Desktop Apps
    spotify
    spotify-tui
    gimp
    steam-tui
    steamPackages.steamcmd
    ];

    # Steam
    programs.steam = {
      enable = true;
      remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
      dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
    };

    # fonts.enableFontDir = true;
    fonts.fonts = with pkgs; [
    nerdfonts
    font-awesome
    ipaexfont
    ];

    # Some programs need SUID wrappers, can be configured further or are
    # started in user sessions.
    # programs.mtr.enable = true;
    # programs.gnupg.agent = {
    #   enable = true;
    #   enableSSHSupport = true;
    # };

    # List services that you want to enable:

    # Enable the OpenSSH daemon.
    # services.openssh.enable = true;

    # Open ports in the firewall.
    # networking.firewall.allowedTCPPorts = [ ... ];
    # networking.firewall.allowedUDPPorts = [ ... ];
    # Or disable the firewall altogether.
    # networking.firewall.enable = false;

    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = "23.05"; # Did you read the comment?
    
    ####
    #### ╔══════╦╦═════╗
    #### ║╔═╦═╦╦╣╠═╦══╗║
    #### ║║║╠╝║║║║║╠╗╚╣║
    #### ║║╔╩═╬╗╠╩═╩══╝║
    #### ╚╩╩══╩═╩══════╝
    ####
    ## HOME-MANAGER CONFIGURATION

    # User Settings:
    # You may set your NixOS username and password here.
    users.users.paylhorse = {
      isNormalUser = true;
      description = "Some user.";
      extraGroups = [ "networkmanager" "wheel" ];
      # psswd = "qwer1234";
    };

    # IMPORTANT
    # Change 'paylhorse' below to your desired username set above, for a complete installation of paylos to your user.
    # Define a user account. Don't forget to set a password with ‘passwd’.
    home-manager.users.paylhorse = { pkgs, ... }: {
      home.packages = [

      ];
      programs.bash.enable = true;

      # Git Installation:
      # You may set your Git username and email here.
      programs.git = {
        enable = true;
        userName  = "paylhorse";
        userEmail = "realalsuwaidi@gmail.com";
      };

      # GTK Theme Configuration:
      gtk = {
        enable = true;

        iconTheme = {
          name = "faba";
          package = pkgs.faba-icon-theme;
        };

        theme = {
          name = "palenight";
          package = pkgs.palenight-theme;
        };

        cursorTheme = {
          name = "mint-cursor";
          package = pkgs.cinnamon.mint-cursor-themes;
        };

        gtk3.extraConfig = {
          Settings = ''
            gtk-application-prefer-dark-theme=1
          '';
        };

        gtk4.extraConfig = {
          Settings = ''
            gtk-application-prefer-dark-theme=1
          '';
        };
      };

      home.sessionVariables.GTK_THEME = "palenight";

      home.file{
        ".config/hypr".source = ./dotfiles/

      };
      home.stateVersion = "23.05";
    };

  }


  