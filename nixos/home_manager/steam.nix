# Build instructions in configuration.nix
{ pkgs, ... }: {
    nixpkgs.config.allowUnfree = true;
    home.packages = with pkgs; [
        cabextract # required for AoE patch
        chromium
        discord
        gnome.nautilus
        keybase-gui
        okular
        signal-desktop
        spotify
        zip
        zotero
    ];

    # Program config
    programs.alacritty.enable = true;
    programs.i3status-rust = {
        enable = true;
    };
    programs.firefox = {
        enable = true;
        profiles.default = {
            id=0;
            isDefault=true;
            name="default";
            settings = {
                # browser.startup  # TODO: Make this continue where was left off.
                "browser.search.isUS" = false;
                "browser.search.region" = "GB";
                "distribution.searchplugins.defaultLocale" = "en-GB";
                "general.useragent.locale" = "en-GB";
            };
        };
    };
    programs.nix-index.enable = true;
    programs.nix-index.enableZshIntegration = true;

    programs.vscode.enable = true;
    programs.zsh = {
        enable = true;
        defaultKeymap = "emacs";
        # autosuggestions.enable = false;
        enableCompletion = true;
        shellAliases = let
            nvim = "${pkgs.neovim}/bin/nvim";
        in {
            ll = "ls -hal";
            ".." = "cd ..";
            vim = nvim;
            vi = nvim;
        };
    };

    # Services config
    services.keybase.enable = true;
    services.kbfs.enable = true;

    # services.dropbox.enable = true;
    # services.dropbox.path = "${config.home.homeDirectory}/Dropbox";

    home.stateVersion = "20.09";
}

# TODO:
#  - configure ssh agent to serve ssh keys from keybase
#  - better alacritty colours
#  - clone and load other dotfiles
#    - vim configuration
#    - i3 config
#    - vscode
#  - messaging apps as services
#  - firefox extensions config with nixos
#  - system python & pip or system python alias to docker container?
#  - pycharm
#  - vscode extensions
#  - screen brightness
#  - programs.light (run without sudo?) + configure light keys
#    - redshift
#  - screen locker
#  - set wm by user?
