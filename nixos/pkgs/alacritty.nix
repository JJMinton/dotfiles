# Based on https://github.com/nix-community/home-manager/blob/master/modules/programs/alacritty.nix
# with wrapper insights from https://nixos.wiki/wiki/Nix_Cookbook#Wrapping_packages
# and bash script based on http://mywiki.wooledge.org/BashFAQ/035

{ alacritty, symlinkJoin, default_config_file, stdenv, writeScriptBin, ... }:
# default_config_file = ${config:-${lib.environment.etc."alacritty/default_config.yaml"}}

symlinkJoin {
  name = "alacritty_wrapper";
  paths = [
    (
      writeScriptBin "alacritty" ''
        #!${stdenv.shell}
        while :; do
          case $1 in
            --config-file)
              config=''${2-}
              shift
              ;;
            --config-file=*)
              config=''${1:14} # assign just the argument part.
              ;;
            ?*)
              arguments="$arguments $1"
              ;;
            *)
              break
          esac
          shift
        done

        if [[ -z ''${config-unset} ]]; then  # if set but null
            exec ${alacritty}/bin/alacritty $arguments
        else
            exec ${alacritty}/bin/alacritty $arguments --config-file=''${config:-${default_config_file}} 
        fi
      ''
    )
    alacritty
  ];
}