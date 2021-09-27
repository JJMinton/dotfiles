# Build instructions in configuration.nix
{ pkgs, ... }: {
    nixpkgs.config.allowUnfree = true;
    home.packages = with pkgs; [
        chromium
        docker-compose
        discord
        imagemagick # for screenshot
        inkscape
        keybase-gui
        libreoffice
        okular
        slack-dark
        signal-desktop
        spotify
        unzip
        vscode
        zip
        zotero
    ];

    # Program config
    programs.alacritty = {
        enable = true;
    };
    #TODO: use programs.alacritty.settings
    home.file.alacrittyConf = {
        source = ../../alacritty/alacritty.yaml;
        target = ".config/alacritty/alacritty.yml";
    };
    programs.firefox = {
        enable = true;
        extensions = [
            # https-everywhere  # Does this require NUR?
            # ublock
            # facebook container
            # multi-account container
            # lastpass
            # vimvixen
            # zotero
        ];
        profiles.default = {
            id=0;
            isDefault=true;
            name="default";
            settings = {
                # browser.startup  # TODO: Make this continue where was left off.
                "browser.search.region" = "GB";
                "browser.search.isUS" = false;
                "distribution.searchplugins.defaultLocale" = "en-GB";
                "general.useragent.locale" = "en-GB";
            };
        };
    };
    programs.nix-index.enable = true;
    programs.nix-index.enableZshIntegration = true;
    programs.vscode = {
        enable = true;
        extensions = with pkgs.vscode-extensions; [
            bbenoist.Nix
            vscodevim.vim
            # eamodio.gitlens
            # ms-vsliveshare.vsliveshare
            # tomoki1207.pdf
            # ms-vscode-remote.vscode-remote
            # ryu1kn.partial-diff
            # rubbersheep.gi
        ];
        keybindings = [];
        userSettings = {};
    };
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
