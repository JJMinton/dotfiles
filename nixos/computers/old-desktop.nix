{ config, lib, pkgs, ... }:
{
  # Other config
  services.xserver.layout = lib.mkDefault "gb";  # Keyboard layout
  boot.loader.efi.efiSysMountPoint = "/efi";
}
