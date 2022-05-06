let

    # Originally from: https://github.com/nix-community/nur-combined/blob/e745144e9650d083bde1c454d4653ba7cdeb9518/repos/rycee/pkgs/firefox-addons/default.nix
  buildFirefoxXpiAddon = { pname, version, addonId, url, sha256, ... }:
    pkgs.stdenv.mkDerivation {
      name = "${pname}-${version}";

      src = builtins.fetchurl { inherit url sha256; };

      preferLocalBuild = true;
      allowSubstitutes = false;

      buildCommand = ''
          dst="$out/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}"
          mkdir -p "$dst"
          install -v -m644 "$src" "$dst/${addonId}.xpi"
      '';
    };


  # extension_var = buildFirefoxXpiAddon {
  #   pname = "extension-name";
  #   version = "version-id";
  #   # To find addonId you need to find the manifest.json of the addon- this might be available in the
  #   # source code, e.g. https://github.com/dessant/search-by-image/blob/37e905336bb420e72724bef6d71c5aa7b2147723/src/manifest/firefox.json
  #   # It might also be possible to download the .xpi file (just a .zip file) at $url below,
  #   # extract it, and examine the manifest.
  #   # It seems to be possible for an extension to lack an id. See save-to-wayback-machine below.
  #   # In this case, it seems as though using any id works, but it may be necessary to
  #   # subsequently "install the add-on manually"- whatever that actually means.
  #   addonId = "{addon-id}";
  #   url = "URL that the [+ Add to Firefox] button on the add-on page will send you to";
  #   sha256 = "<sha256>";
  # };
  https-everywhere = buildFirefoxXpiAddon {
    pname = "https-everywhere";
    version = "2021.7.13";
    addonId = "https-everywhere@eff.org";
    url = "https://addons.mozilla.org/firefox/downloads/file/3809748/https_everywhere-2021.7.13-an+fx.xpi";
    sha256 = "e261461b5d4d3621285fce70773558184d691c614b330744dab672f032db731c";
    meta = with lib;
    {
      homepage = "https://www.eff.org/https-everywhere";
      description = "Encrypt the web! HTTPS Everywhere is a Firefox extension to protect your communications by enabling HTTPS encryption automatically on sites that are known to support it, even when you type URLs or follow links that omit the https: prefix.";
      license = {
        shortName = "https-everywhere-license";
        fullName = "Multiple";
        url = "https://addons.mozilla.org/en-US/firefox/addon/https-everywhere/license/";
        free = true;
        };
      platforms = platforms.all;
    };
  };
  ublock-origin = buildFirefoxXpiAddon {
    pname = "ublock-origin";
    version = "1.41.8";
    addonId = "uBlock0@raymondhill.net";
    url = "https://addons.mozilla.org/firefox/downloads/file/3913320/ublock_origin-1.41.8-an+fx.xpi";
    sha256 = "527c7527116a6c6ffddb963cdfb901518d56f624cfc922f6eab32916b24e8f5d";
    meta = with lib;
    {
      homepage = "https://github.com/gorhill/uBlock#ublock-origin";
      description = "Finally, an efficient wide-spectrum content blocker. Easy on CPU and memory.";
      license = licenses.gpl3;
      platforms = platforms.all;
    };
  };
  facebook-container = buildFirefoxXpiAddon {
    pname = "facebook-container";
    version = "2.3.2";
    addonId = "@contain-facebook";
    url = "https://addons.mozilla.org/firefox/downloads/file/3923300/facebook_container-2.3.2-fx.xpi";
    sha256 = "a1851f15ae4ec790c40f9a751ad6d64a44a6bf47f70ee497ef4ee17115bb7e06";
    meta = with lib;
    {
      homepage = "https://github.com/mozilla/contain-facebook";
      description = "Prevent Facebook from tracking you around the web. The Facebook Container extension for Firefox helps you take control and isolate your web activity from Facebook.";
      license = licenses.mpl20;
      platforms = platforms.all;
    };
  };
  multi-account-containers = buildFirefoxXpiAddon {
    pname = "multi-account-containers";
    version = "8.0.6";
    addonId = "@testpilot-containers";
    url = "https://addons.mozilla.org/firefox/downloads/file/3907697/firefox_multi_account_containers-8.0.6-fx.xpi";
    sha256 = "d93db0b146ef6982011200e58b8afa9c43bdc345116333593826667c5db5cfab";
    meta = with lib;
    {
      homepage = "https://github.com/mozilla/multi-account-containers/#readme";
      description = "Firefox Multi-Account Containers lets you keep parts of your online life separated into color-coded tabs. Cookies are separated by container, allowing you to use the web with multiple accounts and integrate Mozilla VPN for an extra layer of privacy.";
      license = licenses.mpl20;
      platforms = platforms.all;
    };
  };
  lastpass-password-manager = buildFirefoxXpiAddon {
    pname = "lastpass-password-manager";
    version = "4.91.0.4";
    addonId = "support@lastpass.com";
    url = "https://addons.mozilla.org/firefox/downloads/file/3925414/lastpass_password_manager-4.91.0.4-an+fx.xpi";
    sha256 = "2d96079f677fac27be1e430966d3d38951f2da8c0675c0772c3b54eca4d2d427";
    meta = with lib;
    {
      homepage = "https://lastpass.com/";
      description = "LastPass, an award-winning password manager, saves your passwords and gives you secure access from every computer and mobile device.";
      license = {
        shortName = "unfree";
        fullName = "Unfree";
        url = "https://addons.mozilla.org/en-US/firefox/addon/lastpass-password-manager/license/";
        free = false;
        };
      platforms = platforms.all;
    };
  };
  vim-vixen = buildFirefoxXpiAddon {
    pname = "vim-vixen";
    version = "1.2.3";
    addonId = "vim-vixen@i-beam.org";
    url = "https://addons.mozilla.org/firefox/downloads/file/3845233/vim_vixen-1.2.3-an+fx.xpi";
    sha256 = "8f86c77ac8e65dfd3f1a32690b56ce9231ac7686d5a86bf85e3d5cc5a3a9e9b5";
    meta = with lib;
    {
      homepage = "https://github.com/ueokande/vim-vixen";
      description = "Accelerates your web browsing with Vim power!!";
      license = licenses.mit;
      platforms = platforms.all;
    };
  };
  vimium = buildFirefoxXpiAddon {
    pname = "vimium";
    version = "1.67.1";
    addonId = "{d7742d87-e61d-4b78-b8a1-b469842139fa}";
    url = "https://addons.mozilla.org/firefox/downloads/file/3898202/vimium_ff-1.67.1-fx.xpi";
    sha256 = "12740802748e7abff8f13014c845db182b5266f280e2f9e22fae0af82789fe6d";
    meta = with lib;
    {
      homepage = "https://github.com/philc/vimium";
      description = "The Hacker's Browser. Vimium provides keyboard shortcuts for navigation and control in the spirit of Vim.\n\nThis is a port of the popular Chrome extension to Firefox.\n\nMost stuff works, but the port to Firefox remains a work in progress.";
      license = licenses.mit;
      platforms = platforms.all;
    };
  };
  zotero = buildFirefoxXpiAddon {
    pname = "extension-name";
    version = "version-id";
    addonId = "{addon-id}";
    url = "URL that the [+ Add to Firefox] button on the add-on page will send you to";
    sha256 = "<sha256>";
  };