# Based on https://github.com/nix-community/home-manager/blob/master/modules/programs/alacritty.nix
# with wrapper insights from https://nixos.wiki/wiki/Nix_Cookbook#Wrapping_packages
# and bash script based on http://mywiki.wooledge.org/BashFAQ/035

{ config, lib, pkgs, callPackage, environment, ... }:

with lib;

let
  cfg = config.programs.alacritty;
  yamlFormat = pkgs.formats.yaml { };
in {
  options = {
    programs.alacritty = {
      enable = mkEnableOption "Alacritty";
      settings = mkOption {
        type = yamlFormat.type;
        default = { };
        example = literalExample ''
          {
            window.dimensions = {
              lines = 3;
              columns = 200;
            };
            key_bindings = [
              {
                key = "K";
                mods = "Control";
                chars = "\\x0c";
              }
            ];
          }
        '';
        description = ''
          Configuration written to <filename>$XDG_CONFIG_HOME/alacritty/alacritty.yml</filename>.
        '';
      };
    };
  };


  config = mkMerge [
    (mkIf (cfg.enable && cfg.settings == { }) {
      environment.systemPackages = [ pkgs.alacritty ];
    })
    (mkIf (cfg.settings != { }) {
      # Apply wrapper that conditionally applies --config-file
      environment.systemPackages = [
        (pkgs.callPackage ../pkgs/alacritty.nix {
          default_config_file = pkgs.writeText "alacritty/default_config.yaml" cfg.settings;
        })
      ];
    })
  ];
}