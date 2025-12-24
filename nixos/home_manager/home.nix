# Build instructions in configuration.nix
{ pkgs, config, ... }: let 
    basicService = {
        desc,
        cmd,
        env ? "",
    }: {
        Unit = {
            Description = desc;
            # TODO: See man-home-configuration.nix for systemd.user.targets
            After = ["network-online.target"];
        };

        Service = {
            ExecStart = cmd;
            KillSignal = "SIGTERM";
            TimeoutStopSec = 5;
            Environment = env;
        };
    };

    constrainedService = {
        cmd,
        cpu ? "100%",
        mem ? "1G",
        desc ? "",
        env ? "",
    }: let
        s = basicService {
            desc = desc;
            cmd = cmd;
            env = env;
        };
        # TODO: is there a way to "deep-replace" below instead of having the awkward .Service
        # replacement?
        in
        s
        // {
        Service =
            s.Service
            // {
            CPUQuota = cpu;
            MemoryMax = mem;
            };
        };
    # TODO: for firefoxService and basicService (run under X11) and chromiumService perhaps we need
    # some X11 configuration. See:
    # - https://github.com/systemd/systemd/blob/v219/NEWS#L194
    # - https://wiki.archlinux.org/title/Systemd/User#DISPLAY_and_XAUTHORITY
    # - https://superuser.com/questions/759759/writing-a-service-that-depends-on-xorg
    firefoxService = {
        name,
        desc,
        url,
        env ? "",
        profile ? name,
    }:
        constrainedService {
            inherit desc;
            cpu = "150%";
            mem = "2G";
            # https://wiki.archlinux.org/title/Firefox#Touchscreen_gestures_and_pixel-perfect_trackpad_scrolling
            env = "MOZ_USE_XINPUT2=1 GTK_IM_MODULE=ibus QT_IM_MODULE=ibus XMODIFIERS=@im=ibus ${env}";
            # For some command-line options see:
            # - https://docs.gtk.org/gtk3/running.html
            # - https://docs.gtk.org/gtk3/x11.html
            # - https://wiki.mozilla.org/Firefox/CommandLineOptions
            # hard-coding https means things won't work for non-https URLs
            cmd = "${pkgs.firefox}/bin/firefox --no-remote --class=${name} -P ${profile} https://${url}";
        };
    dropboxDirectory = "${config.home.homeDirectory}/Dropbox";
    in {
    nixpkgs.config.allowUnfree = true;
    home.packages = with pkgs; [
        awscli2
        chromium
        # cura # 3d printing slicer # https://github.com/NixOS/nixpkgs/issues/186570
        darktable
        discord
        ## Dropbox
        # dropbox-cli
        maestral
        maestral-gui  # alternative dropbox client
        docker-compose
        # feh
        cheese  # take images etc. from webcam
        nautilus
        imagemagick  # for screenshot
        inkscape
        keybase-gui
        libreoffice
        meld  # diffing tool
        ncdu  # disk usage explorer
        notify-osd
        nufraw-thumbnailer  # raw thumbnailer
        kdePackages.okular
        kdePackages.kdenlive
        openvpn
        playerctl  # for media keys
        slack
        signal-desktop
        spotify
        telegram-desktop
        teams-for-linux  # Microsoft teams
        tldr  # manual
        transmission_4  # torrent
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
    # Install NUR
    # Used for firefox extensions
    nixpkgs.config.packageOverrides = pkgs: {
        nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
            inherit pkgs;
        };
        # TODO: import only firefox addons stuff
        # firefox-addons = { url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons"; inputs.nixpkgs.follows = "nixpkgs"; };
    };

    # Window manager
    xsession.windowManager.i3 = {
        enable = true;
        package = pkgs.i3;
        config = {
            modifier = "Mod4";
            gaps = {
                inner = 10;
                outer = 5;
            };
        };
    };

    # Program config
    programs.autorandr.hooks = {
        postswitch."change-background" = "systemctl --user start random-background";
    };
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
            keyboard.bindings = [
                { key = "C"; mods = "Control|Shift"; action = "Copy"; }
                { key = "V"; mods = "Control|Shift"; action = "PasteSelection"; }
                { key = "V"; mods = "Control"; action = "Paste"; }
                { key = "Insert"; mods = "Shift"; action = "PasteSelection"; }
                { key = "Return"; mods = "Control|Shift"; action = "SpawnNewInstance"; }
                # { key = "Left"; mods = "Control"; chars = "\\eb"; }
                # { key = "Right"; mods = "Control"; chars = "\\ef"; }
                # { key = "Home"; action = "First"; }
                # { key = "End"; action = "Last"; }
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
                "browser.startup.page" = 3;
                "browser.search.defaultenginename" = "DuckDuckGo";
                "browser.search.order.1" = "DuckDuckGo";
                "browser.search.region" = "GB";
                "browser.search.isUS" = false;
                "browser.search.suggest.enabled" = false;
                "distribution.searchplugins.defaultLocale" = "en-GB";
                "general.useragent.locale" = "en-GB";
                "signon.rememberSignons" = false;
                "media.autoplay.default" = 1;
                "toolkit.cosmeticAnimations.enabled" = false; 
                "datareporting.healthreport.uploadEnabled" = false;
                "toolkit.telemetry.server" = "";
                "network.prefetch-next" = false;
                "dom.webnotifications.enabled" = false;
            };
            extensions = {
                packages = with pkgs.nur.repos.rycee.firefox-addons; [
                    ublock-origin
                    #TODO: keeper
                    #TODO: use custom firefox_addons
                    # https-everywhere  # Does this require NUR?
                    # facebook container
                    # multi-account container
                    # lastpass
                    # vimium
                    # zotero
                ];
                # settings."uBlock@raymondhill.net".settings = {
                #     selectedFilterLists = [
                #         "ublock-filters"
                #         "ublock-badware"
                #         "ublock-privacy"
                #         "ublock-unbreak"
                #         "ublock-quick-fixes"
                #     ];
                # };
            };
            search = {
                force = true;
                default = "ddg";
                order = [ "ddg" "google" ];
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
        lfs.enable = true;
    };
    programs.jujutsu = {
        enable = true;
        settings = {};
    };

    programs.nix-index.enable = true;
    programs.nix-index.enableZshIntegration = true;
    programs.ssh = {
        enable = true;
        includes = ["${config.home.homeDirectory}/.ssh/*/*.config"];
    };
    programs.vscode = {
        enable = true;
        profiles.default = {
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
                github.copilot
                # rubbersheep.gi
                # ace jumper
            ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
                # Script to get entries for installed extensions:
                # https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/editors/vscode/extensions/update_installed_exts.sh
                # {
                #     name = "vscode-remote-extensionpack";
                #     publisher = "ms-vscode-remote";
                #     version = "0.26.0";
                #     sha256 = "19xm7ic29q8sib02y9143gq8x54bm0mkkl65avbmbhlk7bdwy9ij";
                # }
            ];
            userSettings = {
                "[nix]"."editor.tabSize" = 2;
                "editor.lineNumbers" = "relative";
            };
            # user = "${config.user}";
            # homeDirectory = "${config.home.homeDirectory}";
            keybindings = [];
            userSettings = {};
        };
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

        # Servicea
        services = {
            ## remote file systems
            keybase.enable = true;
            # keybase file system
            kbfs = {  # TODO: this isn't working; fix it
                enable = true;
                mountPoint = "${config.home.homeDirectory}/keybase";
            };

            ## Visuals
            picom = {
                enable = true;
                fade = true;
                inactiveOpacity = 0.9;
            };
            random-background = {
                enable = true;
                imageDirectory = "${dropboxDirectory}/Pictures/backgrounds";
            };
            screen-locker = {
                enable = true;
                lockCmd = "${pkgs.i3lock}/bin/i3lock -n -c 000000";
            };

            ## Hotkeys
            playerctld.enable = true;  # to control media players with hotkeys
        };

        systemd.user.services.dropbox  =  { 
            Unit  =  { 
                Description  =  "Dropbox service" ; 
            }; 
            Install  =  { 
                WantedBy  =  [  "default.target"  ]; 
            }; 
            Service  =  { 
                ExecStart  =  " ${pkgs.dropbox}/bin/dropbox" ; 
                Restart  =  "on-failure" ; 
            }; 
        }; 
        systemd.user.services.protonmail = firefoxService{
            name = "protonmail";
            desc = "ProtonMail";
            url = "mail.protonmail.com";
        };



        ## File type associations
        xdg = let 
            filemanager    = "nautilus";
            torrent     = "transmission";
            browser    = "firefox";
        in {
            enable = true;
            desktopEntries = rec {
                "nautilus" = {
                    name        = "Nautilus file manager";
                    genericName = "File Manager";
                    exec        = "${pkgs.nautilus}/bin/nautilus %U";
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
                "text/html"                         = "${browser}.desktop";
                "x-scheme-handler/http"             = "${browser}.desktop";
                "x-scheme-handler/https"            = "${browser}.desktop";
                "x-scheme-handler/about"            = "${browser}.desktop";
                "x-scheme-handler/unknown"          = "${browser}.desktop";
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

        home.stateVersion = "24.11";
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
    #  - setup devpi as service

    #  - setup custom mergetool
    #  - vscode git-diff-and-merge-tool extension