# Build instructions in configuration.nix
{ pkgs, config, ... }: {

    # TODO: setup gui window manager
    # services.xserver.windowManager.gnome.enable = true;  # TODO: 
    # xsession.enable = true;
    # xsession.windowManager.GNOME.enable = true;
    nixpkgs.config.allowUnfree = true;
    home.packages = with pkgs; [
        chromium
        darktable
        discord
        docker-compose
        gnome.cheese
        gnome.nautilus
        imagemagick # for screenshot
        inkscape
        keybase-gui
        libreoffice
        libsForQt5.kdenlive
        okular
        openvpn
        signal-desktop
        spotify
        unzip
        vscode
        zip
    ];

    # Program config
    programs.alacritty.enable = true;
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
        ];
        profiles.default = {
            id=0;
            isDefault=true;
            name="default";
            settings = {
                # browser.startup  # TODO: Make this start a new session
                "browser.search.isUS" = false;
                "browser.search.region" = "GB";
                "distribution.searchplugins.defaultLocale" = "en-GB";
                "general.useragent.locale" = "en-GB";
            };
        };
    };
    programs.nix-index.enable = true;
    programs.nix-index.enableZshIntegration = true;
    programs.zsh = {
        enable = true;
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

}