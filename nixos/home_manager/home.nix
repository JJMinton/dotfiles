# Build instructions in configuration.nix
{ pkgs, config, ... }: {
    nixpkgs.config.allowUnfree = true;
    home.packages = with pkgs; [
        awscli2
        chromium
        darktable
        discord
        docker-compose
        gnome.cheese
        gnome.nautilus
        i3lock  # for screen lock
        imagemagick  # for screenshot
        inkscape
        keybase-gui
        libreoffice
        libsForQt5.kdenlive
        meld  # diffing tool
        ncdu  # disk usage explorer
        notify-osd
        okular
        openvpn
        playerctl  # for media keys
        slack-dark
        signal-desktop
        spotify
        tdesktop  # telegram
        tldr  # manual
        unzip
        vscode
        zip
        zotero
        xclip  # clipboard CLI command
        xorg.xhost
    ];

    # Program config
    programs.alacritty = {
        enable = true;
        settings = {
            env.TERM = "linux";
            font.size = 6.0;
            colors = {# (Trim-yer-beard)
            # Default colors
                primary = {
                    background = "0x191716";
                    foreground = "0xdaba8b";
                };
                # Normal colors
                normal = {
                    black =   "0x0f0e0d";
                    red =     "0x845336";
                    green =   "0x57553c";
                    yellow =  "0xa17e3e";
                    blue =    "0x43454f";
                    magenta = "0x604848";
                    cyan =    "0x5c6652";
                    white =   "0xa18b62";
                };
                # Bright colors
                bright = {
                    black =   "0x383332";
                    red =     "0x8c4f4a";
                    green =   "0x898471";
                    yellow =  "0xc8b491";
                    blue =    "0x65788f";
                    magenta = "0x755e4a";
                    cyan =    "0x718062";
                    white =   "0xbc9d66";
                };
            };
            window.opacity = 0.9;
            key_bindings = [
                {
                    key = "C";
                    mods = "Control|Shift";
                    action = "Copy";
                }
                {
                    key = "Insert";
                    mods = "Shift";
                    action = "PasteSelection";
                }
            ];
        };
    };

    programs.broot = {
        enable = true;
        enableZshIntegration = true;
        settings.verbs = [
        {
            execution = "$EDITOR {directory}/{subpath}";
            invocation = "create {subpath}";
        }
        {
            key = "ctrl-u";
            internal = ":input_clear";
        }
        {
            key = "ctrl-w";
            internal = ":input_del_word_left";
        }
        {
            key = "ctrl-p";
            internal = ":toggle_preview";
        }
        {
            key = "ctrl-h";
            internal = ":toggle_hidden";
        }
        {
            invocation = "edit";
            key = "enter";
            external = "${pkgs.neovim}/bin/nvim {file}";
            leave_broot = false;
            apply_to = "file";
        }
        ];
    };
    programs.i3status-rust = {
        enable = true;
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
    programs.git = {
        enable = true;
        userName = "Jeremy Minton";
        userEmail = "jeremyminton@pm.me";
        extraConfig = {
            # Useful for extraConfig: https://git-scm.com/book/en/v2/Customizing-Git-Git-Configuration
            diff.tool = "vimdiff2";
            diff.algorithm = "histogram";
            difftool.prompt = "false";

            merge.tool = "vimdiff";
            merge.conflictstyle = "diff3";
            mergetool.prompt = "true";
            # from: https://www.linkedin.com/pulse/how-set-up-3-way-merge-tool-git-p4-vimdiff-wasin-thonkaew
            "mergetool \"vimdiff\"".cmd = "nvim -d \"$MERGED\" \"$LOCAL\" \"$BASE\" \"REMOTE\" -c \"wincmd J\" -c \"windo set wrap\"";

            url = { "ssh://git@github.com" = { insteadOf = "https://github.com"; } ; } ;
            url = { "ssh://git@bitbucket.org" = { insteadOf = "https://bitbucket.org"; } ; } ;
            color.ui = "true";
            pull.rebase = "false";
            credential.helper = "libsecret";
        };
    };

    programs.nix-index.enable = true;
    programs.nix-index.enableZshIntegration = true;
    programs.vscode = {
        enable = true;
        extensions = with pkgs.vscode-extensions; [
            vscodevim.vim
            # vscode-pdf
            # ryu1kn.partial-diff
            # rubbersheep.gi
            # ace jumper
        ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
            {
                name = "vscode-remote-extensionpack";
                publisher = "ms-vscode-remote";
                version = "0.21.0";
                sha256 = "14l8h84kvnkbqwmw875qa6y25hhxvx1dsg0g07gdl6n8cv5kvy2g";
                # TODO: lib.fakeSha256  # Should print hash on failure
            }
            {
                name = "vsliveshare";
                publisher = "ms-vsliveshare";
                version = "1.0.5449";
                sha256 = "1i7qn2v0s21pqfdrs9g9cz3f2ydxq763vlfxvp98x38x65zmd920";
            }
            {
                name = "python";
                publisher = "ms-python";
                version = "2022.3.10811002";
                sha256 = "0fmfm2w7xam1jfgrm4nxaiq0w8qwyx51gmw1yd4xfnvh2vdrli1a";
            }
            {
                name = "gitlens";
                publisher = "eamodio";
                version = "12.0.5";
                sha256 = "0zfawv9nn88x8m30h7ryax0c7p68najl23a51r88a70hqppzxshw";
            }
        ];
        # user = "${config.user}";
        # homeDirectory = "${config.home.homeDirectory}";
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
            broot = "${pkgs.broot}/bin/broot";
        in {
            ll = "ls -hal";
            ".." = "cd ..";
            vim = nvim;
            vi = nvim;
            notes = "broot ${config.home.homeDirectory}/repos/dotfiles/notes";
        };
    };

    # Services config
    services.keybase.enable = true;
    services.kbfs.enable = true;


    services.picom = {
        enable = true;
        fade = true;
        inactiveOpacity = 0.8;
    };
    services.random-background = {
        enable = true;
        imageDirectory = "${config.home.homeDirectory}/Dropbox/Pictures/backgrounds";
    };
    services.screen-locker = {
        enable = true;
        lockCmd = "${pkgs.i3lock}/bin/i3lock -n -c 000000";
    };

    services.dropbox.enable = true;
    services.dropbox.path = "${config.home.homeDirectory}/Dropbox";

    services.playerctld.enable = true;  # to control media players with hotkeys

    # xdg.mimeApps.enable = true;
    # xdg.mimeApps.defaultApplications = {
    #     "inode/directory" = "nautilus.desktop";
    #     "application/pdf" = "firefox.desktop";
    # };

    home.stateVersion = "20.09";
}

# TODO:
#  - configure ssh agent to serve ssh keys from keybase
#  - better alacritty colours
#  - clone and load other dotfiles
#    - vim configuration
#    - i3 config
#  - messaging apps as services
#  - firefox extensions config with nixos
#  - system python & pip or system python alias to docker container?
#  - pycharm
#  - programs.light (run without sudo?)
#  - redshift
#  - screen locker
#  - set wm by user?

#  - setup custom mergetool
#  - vscode git-diff-and-merge-tool extension
#  - autorandr