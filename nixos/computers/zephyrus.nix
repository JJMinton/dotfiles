
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

  hardware.pulseaudio.enable = true;

  # GPU configuration
  # Most of it is done with nixos-hardware
  # Enable OpenGL
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };

  hardware.nvidia = { 
    open = true;
    modesetting.enable = true;
    nvidiaPersistenced = false;  # TODO: want to re-enable
    nvidiaSettings = true; 
    package = config.boot.kernelPackages.nvidiaPackages.stable; 
  };
  hardware.nvidia-container-toolkit.enable = true;
  services.xserver.videoDrivers = ["nvidia"];
  systemd.services.nvidia-powerd.enable = false;  # TODO: want to re-enable

  networking.interfaces.wlp2s0.useDHCP = true;

  services.xserver = {
    # Keyboard layout
    xkb.layout = lib.mkDefault "gb";

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
  services.pipewire.enable = false;

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
          "HDMI-A-0" = "00ffffffffffff006a6300160000000001210103802315782aee91a3544c99260f5054210800d1c0d10081009500a9000101010101019c6800a0a04029603020350058d71000001a000000fc00446973706c61790a2020202020000000ff000a202020202020202020202020000000fd0030780fde46000a202020202020018c020340f246900102030410e200d5e305c00023097f0783010000e50f00000c0067030c001000187867d85dc401788801e6060501626200681a000001013078e672d000a0a04029603020350058d71000001a565e00a0a0a029503020350058d71000001a70c200a0a0a055503020350058d71000001a0000000000000000005b";
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


  ######### Power saving from https://github.com/TechsupportOnHold/Batterylife/blob/main/laptop.nix ###############
  # Better scheduling for CPU cycles - thanks System76!!!
  # services.system76-scheduler.settings.cfsProfiles.enable = true;

  # Enable TLP (better than gnomes internal power manager)
  services.tlp = {
    enable = true;
    settings = {
      CPU_BOOST_ON_AC = 1;
      CPU_BOOST_ON_BAT = 0;
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
    };
  };

  services.upower = {
    enable = true;
  };

  # Disable GNOMEs power management
  services.power-profiles-daemon.enable = false;

  # Enable powertop
  powerManagement.powertop.enable = true;
  #################################################################################################################

}