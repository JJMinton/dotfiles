{ config, lib, pkgs, ... }:
{
  # Legacy BIOS/MBR install profile.
  boot.loader.systemd-boot.enable = false;
  boot.loader.grub = {
    configurationLimit = 3;
    efiSupport = false;
    device = "/dev/sda";
  };

  services.xserver.xkb.layout = lib.mkDefault "gb";  # Keyboard layout
}
