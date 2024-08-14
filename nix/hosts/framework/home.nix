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
      nixfmt
      hurl
      gh
      libnotify

      firefox
      discord
      steam
      bitwarden
      nautilus
      obs-studio
      vlc
      easyeffects
      thunderbird
      signal-desktop
      kdeconnect
      (prismlauncher.override { jdks = [ jdk22 jdk8 ]; })
      tidal-hifi
      zotero
      deadd-notification-center

      emacs
      alacritty

      mu
      msmtp

      semgrep

      ruff
      ruff-lsp

      jdk22

      noto-fonts
      fira-code
      open-sans
      inter
      roboto-mono
      jetbrains-mono
      noto-fonts-cjk-sans
      libertine
      roboto
    ];
    emacsPackages = with pkgs.emacsPackages; [ mu4e ];
    pythonPackages = with pkgs.python312Packages; [ python-lsp-server ];
  in packages ++ emacsPackages ++ pythonPackages;

  xresources.properties = {
    "Xft.hinting" = 1;
    "Xft.hintstyle" = "hintslight";
  };

  ### Services

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

  ### Programs
  programs.home-manager.enable = true;
  programs.fish.enable = true;
  programs.mu.enable = true;

  programs.alacritty = {
    enable = true;
    settings = {
      shell = "fish";
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

  programs.gh = {
    enable = true;
    settings.editor = "emacsclient";
  };
}
