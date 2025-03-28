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

      niri
      fuzzel
      mako
      waybar
      swaybg
      swaylock
      xwayland
      xwayland-satellite
      gammastep

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

      emacs30-pgtk
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
      font-awesome

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

  ### Services

  services.pasystray.enable = true;
  services.easyeffects.enable = true;

  # Compositor
  services.picom = {
    enable = true;
    backend = "glx";
    activeOpacity = 1;
    inactiveOpacity = 1;
  };

  services.emacs = {
    enable = true;
    defaultEditor = true;
    package = pkgs.emacs30-pgtk;
  };

  services.gammastep = {
    enable = true;
    dawnTime = "6:00-7:00";
    duskTime = "20:00-21:00";
    temperature = {
      day = 6500;
      night = 3500;
    };
  };

  services.mako = {
    enable = true;
    defaultTimeout = 10000;
    margin = "40,20";
  };

  ### Programs
  programs.home-manager.enable = true;
  programs.mu.enable = true;

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
      merge.conflictstyle = "zdiff3";
      diff.algorithm = "histogram";
    };
  };

  programs.direnv = {
    enable = true;
    enableFishIntegration = true;
    nix-direnv.enable = true;
  };

  programs.alacritty = {
    enable = true;
    settings = {
      terminal.shell = "fish";
      font.size = 10;
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

  programs.gh = {
    enable = true;
    settings.editor = "emacsclient";
  };

  programs.firefox = {
    enable = true;
    policies = { WebsiteFilter = { Block = [ "*://*.facebook.com/" ]; }; };
  };

  programs.swaylock = {
    enable = true;
    settings = {
      color = "000000";
    };
  };
}
