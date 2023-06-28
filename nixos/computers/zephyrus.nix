
{ config, pkgs, lib, callPackage, ... }: 

{

  # Use the systemd-boot EFI boot loader.
  boot.loader.efi = {
    canTouchEfiVariables = true;
    efiSysMountPoint = "/boot/efi";
  };

  imports = [
      # optimised hardware config from https://github.com/NixOS/nixos-hardware
      <nixos-hardware/asus/zephyrus/ga401>
  ];

  # Temperature management
  services.auto-cpufreq.enable = true;

  # GPU configuration
  hardware.nvidia = { 
    package = config.boot.kernelPackages.nvidiaPackages.latest; 
    prime = { 
      offload = { 
        enable = true; 
        enableOffloadCmd = true; 
      }; 
    }; 
    powerManagement = { 
      enable = true; 
      finegrained = true; 
    }; 
    nvidiaPersistenced = true; 
    nvidiaSettings = true; 
  };

  networking.interfaces.wlp2s0.useDHCP = true;

  services.xserver = {
    videoDrivers = ["nvidia"];
    # Keyboard layout
    layout = lib.mkDefault "gb";

    # Screen size
    xrandrHeads = [
      {
        output = "eDP-1";
        primary = true;
        monitorConfig = ''
          DisplaySize 309 174
        '';
      }
    ];
  };
  virtualisation.docker.enableNvidia = true;

}