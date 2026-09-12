# Home Manager user services.
#
# This module provides shared builders for constrained and graphical services,
# then exposes these services as opt-in options:
#
#   my-services.gcalendar.enable
#   my-services.gmail.enable
#   my-services.outlook.enable
#   my-services.outlookcalendar.enable
#   my-services.protonmail.enable
#   my-services.whatsapp.enable
#
# Enable services from home.nix with, for example:
#
#   my-services.protonmail.enable = true;
#
# Each enabled option creates a systemd user service. Services start with the
# graphical session and inherit the CPU and memory limits defined below.
{ config, lib, pkgs, ... }:

let
  basicService = {
    desc,
    cmd,
    env ? "",
  }:
    {
      Unit = {
        Description = desc;
        After = [ "network-online.target" ];
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
  }:
    let
      service = basicService { inherit desc cmd env; };
    in
      service // {
        Service = service.Service // {
          CPUQuota = cpu;
          MemoryMax = mem;
        };
      };

  graphicalService = service:
    service // {
      Unit = service.Unit // {
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };

  firefoxService = {
    name,
    desc,
    url,
    env ? "",
    profile ? name,
  }:
    graphicalService (constrainedService {
      inherit desc;
      cpu = "150%";
      mem = "2G";
      env = "MOZ_USE_XINPUT2=1 GTK_IM_MODULE=ibus QT_IM_MODULE=ibus XMODIFIERS=@im=ibus ${env}";
      cmd = "${pkgs.firefox}/bin/firefox --no-remote --class=${name} -P ${profile} https://${url}";
    });

  chromiumService = {
    name,
    description,
    appUrl,
    profileDirectory,
    className,
    cpuQuota ? "150%",
    memoryMax ? "2G",
  }:
    graphicalService (constrainedService {
      desc = description;
      cpu = cpuQuota;
      mem = memoryMax;
      cmd = "${pkgs.chromium}/bin/chromium --app=${appUrl} --class=${className} --user-data-dir=%h/.config/${profileDirectory}";
    });

  serviceDefinitions = {
    gcalendar = chromiumService {
      name = "gcalendar";
      description = "Google Calendar";
      appUrl = "https://calendar.google.com";
      profileDirectory = "chromium_gcalendar";
      className = "gcalendar";
    };

    gmail = chromiumService {
      name = "gmail";
      description = "Gmail";
      appUrl = "https://mail.google.com";
      profileDirectory = "chromium_gmail";
      className = "gmail";
    };

    outlook = chromiumService {
      name = "outlook";
      description = "Outlook";
      appUrl = "https://outlook.office.com";
      profileDirectory = "chromium_outlook";
      className = "outlook";
    };

    outlookcalendar = chromiumService {
      name = "outlookcalendar";
      description = "Outlook Calendar";
      appUrl = "https://outlook.office365.com/owa/?realm=eigentech.com&exsvurl=1&ll-cc=2057&modurl=1&path=/calendar/view/Week";
      profileDirectory = "chromium_outlookcal";
      className = "outlookcal";
    };

    whatsapp = chromiumService {
      name = "whatsapp";
      description = "WhatsApp";
      appUrl = "https://web.whatsapp.com";
      profileDirectory = "chromium_whatsapp";
      className = "whatsapp";
      cpuQuota = "50%";
      memoryMax = "1G";
    };

    protonmail = firefoxService {
      name = "protonmail";
      desc = "ProtonMail";
      url = "mail.protonmail.com";
    };
  };

  enabledService = name: lib.mkIf config.my-services.${name}.enable {
    ${name} = serviceDefinitions.${name};
  };
in
{
  options.my-services = {
    gcalendar.enable = lib.mkEnableOption "Google Calendar";
    gmail.enable = lib.mkEnableOption "Gmail";
    outlook.enable = lib.mkEnableOption "Outlook";
    outlookcalendar.enable = lib.mkEnableOption "Outlook Calendar";
    whatsapp.enable = lib.mkEnableOption "WhatsApp";
    protonmail.enable = lib.mkEnableOption "ProtonMail";
  };

  config.systemd.user.services = lib.mkMerge [
    (enabledService "gcalendar")
    (enabledService "gmail")
    (enabledService "outlook")
    (enabledService "outlookcalendar")
    (enabledService "whatsapp")
    (enabledService "protonmail")
  ];
}
