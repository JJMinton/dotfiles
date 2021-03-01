# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, callPackage, ... }: 

{

  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
      # ./modules/xdg.nix
      ./modules/alacritty.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # networking.hostName = "nixos"; # Define your hostname.
  networking.wireless.enable = false;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;
    

  # Set your time zone.
  time.timeZone = "Europe/London";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp3s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # Enable the i3 desktop environment.
  services.xserver = {
    enable = true;

    # Keyboard layout
    layout = "gb";
    # Enable touchpad support (enabled default in most desktopManager).
    libinput.enable = true;

    desktopManager.xterm.enable = false;
    displayManager.defaultSession = "none+i3";
    displayManager.lightdm.enable = true;
    windowManager.i3 = {
      enable = true;
      configFile = ../i3/config;
      extraPackages = with pkgs; [
        dmenu #application launcher most people use
        i3status # gives you the default i3 status bar
        i3lock #default i3 screen locker
        i3blocks #if you are planning on using i3blocks over i3status
     ];
    };
  };
  # Enable the GNOME 3 Desktop Environment.
  # services.xserver.enable = true;
  # services.xserver.displayManager.gdm.enable = true;
  # services.xserver.desktopManager.gnome3.enable = true;
  
  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;


  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.jeremy = {
    isNormalUser = true;
    shell = pkgs.zsh;
    home = "/home/jeremy";
    description = "Jeremy Minton";
    extraGroups = [ "wheel" "sudo" "docker" ]; # Enable ‘sudo’ for the user.
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  nixpkgs.config.allowUnfree = true;
  environment.shellAliases = { ll = "ls -hal"; };
  environment.variables = {
    EDITOR = "${pkgs.neovim}/bin/nvim";
    VISUAL = "${pkgs.neovim}/bin/nvim";
    TERMINAL = "alacritty"; #envvar for i3  # TODO: refernece this from a packate; requires merging custom packages with nixpkgs
  };

  # For i3: https://nixos.wiki/wiki/I3
  environment.pathsToLink = [ "/libexec" ]; # links /libexec from derivations to /run/current-system/sw 
  environment.systemPackages = with pkgs; [
    alacritty
    arandr
    docker
    firefox
    git
    libreoffice
    neovim
    pavucontrol
    signal-desktop
    slack-dark
    spotify
    vscode
    wget
    zsh
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };
  # programs.neovim = {
  #   enable = true;
  #   viAlias = true;
  #   vimAlias = true;
  #   extraConfig = (builtins.readFile ~/.dotfiles/vimrc/vimrc);
  # };
  programs.zsh = {
    enable = true;
    autosuggestions.enable = false;
    enableCompletion = true;
  };
  programs.alacritty = {
    enable = true;
    settings = builtins.readFile ../alacritty/alacritty.yaml;
  };

  # List services that you want to enable:
  services.keybase.enable = true;
  services.kbfs.enable = true;

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;
  # security.sudo.extraRules = [
  #   { groups = [ "sudo" ]; commands = [ "ALL" ]; };
  # ];
  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

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
  system.stateVersion = "20.09"; # Did you read the comment?

  virtualisation.docker.enable = true;

}


# TODO:
#  - alacritty yaml configuraiton by default
#  - clone and load other dotfiles
#    - vim configuration
#  - messaging apps
#  - keybase file system
#  - i3 config to nixos config
#  - vim config to nixos config
#  - firefox extensions config with nixos
#  - system python & pip
#  - system python alias to docker container?
#  - screen brightness
#  - redshift
#  - screen locker
