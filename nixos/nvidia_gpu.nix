{ config, lib, pkgs, ... }:
{
  nixpkgs.config.allowUnfree = true;  
  services.xserver.videoDrivers = [ "nvidia" ];
}