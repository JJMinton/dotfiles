# Build instructions in configuration.nix
{ pkgs, config, ... }: {
    nixpkgs.config.allowUnfree = true;
    home.packages = with pkgs; [
        awscli2
        chromium
        darktable
        discord
        docker-compose
        feh
        gnome.cheese
        gnome.nautilus
        imagemagick  # for screenshot
        inkscape
        keybase-gui
        libreoffice
        libsForQt5.kdenlive
        meld  # diffing tool
        ncdu  # disk usage explorer
        notify-osd
        nufraw-thumbnailer  # raw thumbnailer
        okular
        openvpn
        playerctl  # for media keys
        slack-dark
        signal-desktop
        spotify
        tdesktop  # telegram
        tldr  # manual
        transmission  # torrent
        transmission-remote-gtk
        unzip
        vlc  # media player
        vscode
        zip
        zotero
        xclip  # clipboard CLI command
        xorg.xhost
        
        # Needed for i3 manager
        dmenu #application launcher most people use
        i3status # gives you the default i3 status bar
        i3lock #default i3 screen locker
        i3blocks #if you are planning on using i3blocks over i3status
    ];

    # Program config
    programs.alacritty = {
        enable = true;
        settings = {
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
                { key = "C"; mods = "Control|Shift"; action = "Copy"; }
                { key = "V"; mods = "Control|Shift"; action = "PasteSelection"; }
                { key = "V"; mods = "Control"; action = "Paste"; }
                { key = "Insert"; mods = "Shift"; action = "PasteSelection"; }
                { key = "Return"; mods = "Control|Shift"; action = "SpawnNewInstance"; }
                { key = "Left"; mods = "Control"; chars = "\\eb"; }
                { key = "Right"; mods = "Control"; chars = "\\ef"; }
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
            extensions = [
                # https-everywhere  # Does this require NUR?
                # ublock
                # facebook container
                # multi-account container
                # lastpass
                # vimvixen
                # zotero
            ];
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
    programs.ssh = {
        enable = true;
        includes = ["${config.home.homeDirectory}/.ssh/*.config" "${config.home.homeDirectory}/.ssh/*/*.config"];
    };
    programs.vscode = {
        enable = true;
        extensions = with pkgs.vscode-extensions; [
            # available packages:
            # https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/editors/vscode/extensions/default.nix
            vscodevim.vim
            eamodio.gitlens
            ms-vsliveshare.vsliveshare
            foam.foam-vscode
            ms-python.python
            ms-toolsai.jupyter
            tomoki1207.pdf
            ryu1kn.partial-diff
            # rubbersheep.gi
            # ace jumper
        ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
            # Script to get entries for installed extensions:
            # https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/editors/vscode/extensions/update_installed_exts.sh
            {
                name = "vscode-remote-extensionpack";
                publisher = "ms-vscode-remote";
                version = "0.24.0";
                sha256 = "0sha4l16x4nn3pxm2g6mxlvm7dz56ry2y3ii4b1s9ilckid0kzpa";
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

    services.dropbox = {
        enable = true;
        path = "${config.home.homeDirectory}/Dropbox";
    };

    services.playerctld.enable = true;  # to control media players with hotkeys

    xdg = let 
        filemanager    = "nautilus";
        torrent     = "transmission";
        browswer    = "firefox";
    in {
        enable = true;
        desktopEntries = rec {
            "nautilus" = {
                name        = "Nautilus file manager";
                genericName = "File Manager";
                exec        = "${pkgs.gnome.nautilus}/bin/nautilus %U";
                terminal    = false;
                categories  = [ "Utility" "FileTools" "FileManager" ];
                mimeType    = [ "inode/directory" ];
            };
            "firefox" = {
                name        = "Firefox web browser";
                genericName = "Web browser";
                exec        = "${pkgs.firefox}/bin/firefox %U";
                terminal    = false;
                categories  = [ "Network" "WebBrowser" ];
                mimeType    = [
                    "x-scheme-handler/about"
                    "x-scheme-handler/unknown"
                    "x-scheme-handler/http"
                    "x-scheme-handler/https"
                    "image/svg+xml"
                ];
            };
            "torrent" = {
                name       = "Transmission torrent";
                genericName = "Torrent client";
                exec        = "${pkgs.transmission-remote-gtk}/bin/transmission-remote-gtk %U";
                terminal    = false;
                categories  = [ "Network" ];
                mimeType    = [
                    "application/x-bittorrent"
                    "x-scheme-handler/magnet"
                ];
            };
        };
        mime.enable = true;
        mimeApps.enable = true;
        mimeApps.defaultApplications = {
            "inode/directory"                   = "${filemanager}.desktop";
            "text/html"                         = "${browswer}.desktop";
            "x-scheme-handler/http"             = "${browswer}.desktop";
            "x-scheme-handler/https"            = "${browswer}.desktop";
            "x-scheme-handler/about"            = "${browswer}.desktop";
            "x-scheme-handler/unknown"          = "${browswer}.desktop";
            # "image/jpeg"                        = "${feh}.desktop";
            # "image/bmp"                         = "${feh}.desktop";
            # "image/png"                         = "${feh}.desktop";
            # "image/tiff"                        = "${feh}.desktop";
            # "image/x-icon"                      = "${feh}.desktop";
            # "image/x-xpixmap"                   = "${feh}.desktop";
            # "image/x-xbitmap"                   = "${feh}.desktop";
            # "application/javascript"            = "${vscode}.desktop";
            # "application/json"                  = "${vscode}.desktop";
            # "application/x-bzip-compressed-tar" = "${vscode}.desktop";
            # "application/x-compressed-tar"      = "${vscode}.desktop";
            # "application/x-shellscript"         = "${vscode}.desktop";
            # "application/zip"                   = "${vscode}.desktop";
            # "text/english"                      = "${vscode}.desktop";
            # "text/plain"                        = "${vscode}.desktop";
            # "text/rust"                         = "${vscode}.desktop";
            "application/x-bittorrent"          = "${torrent}.desktop";
            "x-scheme-handler/magnet"           = "${torrent}.desktop";
        };
    };

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
#  - set wm by user?

#  - setup custom mergetool
#  - vscode git-diff-and-merge-tool extension
#  - autorandr