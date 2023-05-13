{ config, lib, pkgs, ... }:
{
  # imports = [common/pc/laptop/acpi_call.nix];

  #common/cpu/intel/cpu-only.nix via common/cpu/intel
  hardware.cpu.intel.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;

  #common/gpu/intel via common/cpu/intel
  boot.initrd.kernelModules = [ "i915" ];

  environment.variables = {
    VDPAU_DRIVER = lib.mkIf config.hardware.opengl.enable (lib.mkDefault "va_gl");
  };

  hardware.opengl.extraPackages = with pkgs; [
    vaapiIntel
    libvdpau-va-gl
    intel-media-driver
  ];

  #common/pc/laptop
  services.tlp.enable = lib.mkDefault ((lib.versionOlder (lib.versions.majorMinor lib.version) "21.05")
                                      || !config.services.power-profiles-daemon.enable);

  #common/pc via common/pc/laptop
  boot.blacklistedKernelModules = lib.optionals (!config.hardware.enableRedistributableFirmware) [
    "ath3k"
  ];

  services.xserver.libinput.enable = lib.mkDefault true;

  #common/pc/ssd via common/pc/laptop/ssd
  services.fstrim.enable = lib.mkDefault true;

  #lenovo/thinkpad/x1 via lenovo/thinkpad/x1/6th-gen
  hardware.trackpoint.enable = lib.mkDefault true;
  hardware.trackpoint.emulateWheel = lib.mkDefault config.hardware.trackpoint.enable;

  # lenovo/thinkpad/x1/6th-gen
  services.throttled.enable = lib.mkDefault true;

  # Other config
  services.xserver.layout = lib.mkDefault "gb";  # Keyboard layout
}