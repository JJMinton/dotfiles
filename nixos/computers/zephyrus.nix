
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
    # TODO: do I need this with autorandr?
    xrandrHeads = lib.mkDefault [
      {
        output = "eDP-1";
        primary = true;
        monitorConfig = ''
          DisplaySize 309 174
        '';
      }
    ];
  };

  services.autorandr = lib.mkDefault {
    enable = lib.mkDefault true;
    profiles = {
      "mobile" = {
        fingerprint = {
          "eDP" = "00ffffffffffff0006af8ce600000000001d0104a51f117803f4f5a4544d9c270f505400000001010101010101010101010101010101e65f00a0a0a040503020350035ae10000018b84c00a0a0a040503020350035ae10000018000000fd00283c5b5b19010a202020202020000000fe004231343051414e30322e33200a00d3";
        };
        config = {
          "eDP" = {
            enable = true;
            crtc = 0;
            mode = "2560x1440";
            position = "0x360";
            primary = true;
            rate = "60.01";
          };
        };
      };
      "mobile+" = {
        fingerprint = {
          "eDP" = "00ffffffffffff0006af8ce600000000001d0104a51f117803f4f5a4544d9c270f505400000001010101010101010101010101010101e65f00a0a0a040503020350035ae10000018b84c00a0a0a040503020350035ae10000018000000fd00283c5b5b19010a202020202020000000fe004231343051414e30322e33200a00d3";
          "HDMI-A-0" = "00ffffffffffff0010ace9a04c5937312a1d0103803d2378eeee95a3544c99260f5054a54b00714f8180a9c0a940d1c0e1000101010108e80030f2705a80b0588a00615d2100001a000000ff00464e38344b3941483137594c0a000000fc0044454c4c205532373138510a20000000fd0031560a893c000a202020202020016b02033ef15861605f5e5d10050402071601141f1213272021220306111523091f07830100006d030c001000307820006003020167d85dc40178c003e20f03565e00a0a0a0295030203500615d2100001a04740030f2705a80b0588a00615d2100001ebf1600a08038134030203a00615d2100001a00000000000000000000002e";
        };
        config = {
          "eDP" = {
            enable = true;
            crtc = 0;
            mode = "2560x1440";
            position = "2560x360";
            primary = true;
            rate = "60.01";
          };
          "HDMI-A-0" = {
            enable = true;
            crtc = 1;
            mode = "2560x1600";
            position = "0x0";
            rate = "60.00";
          };
        };
      };
    };
  };
  
  virtualisation.docker.enableNvidia = true;

}