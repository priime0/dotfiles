{ inputs, configDir, config, pkgs, lib, ... }: {
  imports = [ ../common/home.nix ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  home.stateVersion = "23.11"; # Did you read the comment?

  home.packages = let
    packages = with pkgs; [
      pavucontrol
      pamixer
      picom
      rofi
      notify-desktop
      pass
      just
      zathura
      libvterm
      texliveFull
      flameshot
      brightnessctl
      pinentry-gtk2
      bluez5-experimental
      bluez-tools
      blueman
      dmenu
      pipewire
      dconf
      mu
      isync
      openssl
      meson
      glib
      stow
      syncthing
      nitrogen
      redshift
      networkmanager
      nixfmt-classic
      hurl
      gh
      libnotify
      tokei
      eza
      typst
      typst-live
      gdb
      ccls
      clang-tools
      valgrind
      fzf
      rofi
      rofi-bluetooth
      docker
      awscli
      bear
      direnv
      wasmtime
      wabt
      gleam
      erlang
      zip
      framework-system-tools

      bspwm
      sxhkd

      discord
      steam
      bitwarden
      nautilus
      obs-studio
      vlc
      easyeffects
      thunderbird
      signal-desktop
      plasma5Packages.kdeconnect-kde
      (prismlauncher.override { jdks = [ jdk23 jdk8 ]; })
      tidal-hifi
      zotero
      deadd-notification-center
      mullvad-browser
      networkmanagerapplet
      pasystray
      arandr
      inkscape
      zoom-us
      gimp
      libreoffice
      peek
      rofimoji
      playerctl

      aspell
      aspellDicts.en

      nil

      emacs30
      alacritty
      emacs-lsp-booster

      mu
      msmtp

      semgrep

      ruff
      ruff-lsp

      irony-server

      typescript-language-server
      prettierd

      jdk23

      ocaml
      ocamlPackages.ocaml-lsp
      dune_3
      opam
      ocamlformat

      noto-fonts
      fira-code
      open-sans
      inter
      roboto-mono
      jetbrains-mono
      noto-fonts-cjk-sans
      libertine
      roboto

      adwaita-icon-theme

      ihaskell
    ];
    emacsPackages = with pkgs.emacsPackages; [ mu4e ];
    pythonPackages = with pkgs.python312Packages; [ python-lsp-server ];
  in packages ++ emacsPackages ++ pythonPackages;

  home.sessionPath = [
    "$HOME/.local/share/racket/8.14/bin/" # This has to be updated every time Racket updates :(
  ];

  home.pointerCursor = {
    gtk.enable = true;
    x11.enable = true;
    name = "Adwaita";
    package = pkgs.adwaita-icon-theme;
    size = 16;
  };

  xresources.properties = {
    "Xft.antialias" = true;
    "Xft.rgba" = "rgb"; # type of anti-aliasing
    "Xft.hinting" = 1; # font hinting
    "Xft.hintstyle" = "hintslight";
  };

  # Window Manager
  xsession.windowManager.bspwm = {
    enable = true;
    monitors = {
      "eDP-2" = [
        "1"
        "2"
        "3"
        "4"
        "5"
        "6"
        "7"
        "8"
        "9"
        "10"
        "11"
        "12"
        "13"
        "14"
        "15"
        "16"
        "17"
        "18"
        "19"
        "20"
      ];
    };
    settings = {
      border_width = 2;
      normal_border_color = "#919191";
      focused_border_color = "FF6054";
      presel_feedback_color = "#CCCCCC";
      window_gap = 25;
      split_ratio = 0.5;
      borderless_monocle = false;
      gapless_monocle = true;
      pointer_follows_monitor = true;
      pointer_follows_focus = false;
      focus_follows_pointer = true;
      merge_overlapping_monitors = true;
      top_monocle_padding = 25;
      right_monocle_padding = 25;
      left_monocle_padding = 25;
      bottom_monocle_padding = 25;
    };
  };

  ### Services

  services.pasystray.enable = true;
  services.easyeffects.enable = true;

  # Hotkey Daemon
  services.sxhkd = {
    enable = true;
    # Reminder: Written in QWERTY
    keybindings = {
      "super + Return" = ''emacsclient -n -c -a ""'';
      "super + control + Return" = "alacritty";
      "super + d" = "rofi -show drun";
      "super + n" = "rofi-bluetooth";
      "super + g" = "rofimoji";
      "super + shift + d" = "flameshot gui";
      "super + shift + q" = "bspc node -k";
      "super + shift + o" = "bspc wm -r";
      # Window focus
      "super + {j,k,l,semicolon}" = "bspc node -f {west,south,north,east}";
      "super + {Left,Down,Up,Right}" = "bspc node -f {west,south,north,east}";

      # Window movement
      "super + shift + {j,k,l,semicolon}" =
        "bspc node -s {west,south,north,east}";
      "super + shift + {Left,Down,Up,Right}" =
        "bspc node -s {west,south,north,east}";

      # Fullscreen
      "super + f" = "bspc node -t ~fullscreen";

      # Center floating window
      "super + i" = "bspc node -v 0 0";

      # Layout control
      "super + w" = "bspc desktop -l next";
      "super + e" = "bspc desktop -l tiled";

      # Toggle floating
      "super + shift + space" = "bspc node -t ~floating";

      # Workspace switching
      "super + Tab" = "bspc desktop -f last";

      # Switch to workspace - first monitor
      "super + 1" = "bspc desktop -f '^1'";
      "super + 2" = "bspc desktop -f '^2'";
      "super + 3" = "bspc desktop -f '^3'";
      "super + 4" = "bspc desktop -f '^4'";
      "super + 5" = "bspc desktop -f '^5'";
      "super + 6" = "bspc desktop -f '^6'";
      "super + 7" = "bspc desktop -f '^7'";
      "super + 8" = "bspc desktop -f '^8'";
      "super + 9" = "bspc desktop -f '^9'";
      "super + 0" = "bspc desktop -f '^10'";

      # Switch to workspace - second monitor
      "super + ctrl + 1" = "bspc desktop -f '^11'";
      "super + ctrl + 2" = "bspc desktop -f '^12'";
      "super + ctrl + 3" = "bspc desktop -f '^13'";
      "super + ctrl + 4" = "bspc desktop -f '^14'";
      "super + ctrl + 5" = "bspc desktop -f '^15'";
      "super + ctrl + 6" = "bspc desktop -f '^16'";
      "super + ctrl + 7" = "bspc desktop -f '^17'";
      "super + ctrl + 8" = "bspc desktop -f '^18'";
      "super + ctrl + 9" = "bspc desktop -f '^19'";
      "super + ctrl + 0" = "bspc desktop -f '^20'";

      # Move container to workspace - first monitor
      "super + shift + 1" = "bspc node -d '^1'";
      "super + shift + 2" = "bspc node -d '^2'";
      "super + shift + 3" = "bspc node -d '^3'";
      "super + shift + 4" = "bspc node -d '^4'";
      "super + shift + 5" = "bspc node -d '^5'";
      "super + shift + 6" = "bspc node -d '^6'";
      "super + shift + 7" = "bspc node -d '^7'";
      "super + shift + 8" = "bspc node -d '^8'";
      "super + shift + 9" = "bspc node -d '^9'";
      "super + shift + 0" = "bspc node -d '^10'";

      # Move container to workspace - second monitor
      "super + ctrl + shift + 1" = "bspc node -d '^11'";
      "super + ctrl + shift + 2" = "bspc node -d '^12'";
      "super + ctrl + shift + 3" = "bspc node -d '^13'";
      "super + ctrl + shift + 4" = "bspc node -d '^14'";
      "super + ctrl + shift + 5" = "bspc node -d '^15'";
      "super + ctrl + shift + 6" = "bspc node -d '^16'";
      "super + ctrl + shift + 7" = "bspc node -d '^17'";
      "super + ctrl + shift + 8" = "bspc node -d '^18'";
      "super + ctrl + shift + 9" = "bspc node -d '^19'";
      "super + ctrl + shift + 0" = "bspc node -d '^20'";

      # Move to adjacent workspaces
      "super + c" = "bspc desktop -f prev.local";
      "super + v" = "bspc desktop -f next.local";
      "super + shift + c" = "bspc node -d prev.local --follow";
      "super + shift + v" = "bspc node -d next.local --follow";

      # Resize mode equivalent - direct resize bindings
      "super + alt + {j,semicolon}" = "bspc node -z {left -20 0,right 20 0}";
      "super + alt + {k,l}" = "bspc node -z {bottom 0 20,top 0 -20}";

      # Volume controls
      "XF86AudioRaiseVolume" = "pamixer --increase 5";
      "XF86AudioLowerVolume" = "pamixer --decrease 5";
      "XF86AudioMute" = "pamixer --toggle-mute";
      "XF86AudioPrev" = "playerctl previous";
      "XF86AudioPlay" = "playerctl play-pause";
      "XF86AudioNext" = "playerctl next";

      # Brightness controls
      "XF86MonBrightnessUp" = "brightnessctl set +5%";
      "XF86MonBrightnessDown" = "brightnessctl set 5%-";

      # Convenient volume/brightness keys
      "super + a" = "pamixer --increase 5";
      "super + s" = "pamixer --decrease 5";
      "super + shift + a" = "brightnessctl set +5%";
      "super + shift + s" = "brightnessctl set 5%-";
    };
  };

  # Compositor
  services.picom = {
    enable = true;
    backend = "glx";
    activeOpacity = 1;
    inactiveOpacity = 0.9;
    settings = {
      blur = {
        method = "kawase";
        size = 20;
        deviation = 5.0;
      };
    };
  };

  services.emacs = {
    enable = true;
    defaultEditor = true;
    package = pkgs.emacs30;
  };

  services.redshift = {
    enable = true;
    tray = true;
    dawnTime = "6:00-7:00";
    duskTime = "20:00-21:00";
    temperature = {
      day = 6500;
      night = 3000;
    };
  };

  ### Programs
  programs.home-manager.enable = true;
  programs.mu.enable = true;

  programs.direnv = {
    enable = true;
    enableFishIntegration = true;
    nix-direnv.enable = true;
  };

  programs.alacritty = {
    enable = true;
    settings = {
      terminal.shell = "fish";
      font.size = 5;
      font.normal = {
        family = "JetBrains Mono";
        style = "SemiBold";
      };
      font.bold = {
        family = "JetBrains Mono";
        style = "Bold";
      };
      font.italic = {
        family = "JetBrains Mono";
        style = "Italic";
      };
      font.bold_italic = {
        family = "JetBrains Mono";
        style = "Bold Italic";
      };
    };
  };

  programs.git = {
    enable = true;
    userName = "Lucas Sta Maria";
    userEmail = "lucas@priime.dev";
    signing = {
      key = "2EAF5BA156A36810B057BF61F07FB16A826E3B47";
      signByDefault = true;
    };
    extraConfig = {
      pull.rebase = true;
      github.user = "priime0";
      credential.helper = "store";
    };
  };

  programs.gh = {
    enable = true;
    settings.editor = "emacsclient";
  };

  programs.firefox = {
    enable = true;
    policies = { WebsiteFilter = { Block = [ "*://*.facebook.com/" ]; }; };
  };
}
