{ inputs, config, pkgs, lib, ... }: {
  imports = [ ./hw.nix ];

  networking.hostName = "framework";
  time.timeZone = "America/New_York";

  # Bootloader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Network Manager
  networking.networkmanager.enable = true;

  # Fonts
  fonts = {
    fontDir.enable = true;
    enableDefaultPackages = true;
    fontconfig = {
      enable = true;
      defaultFonts = { monospace = [ "Roboto Mono" ]; };
    };
    packages = with pkgs; [
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
    ];
  };

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Audio
  services.pipewire = {
    enable = true;
    pulse.enable = true;
  };

  # AMD
  hardware.cpu.amd.updateMicrocode = true;
  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      mesa
      libva
      libvdpau-va-gl
      vulkan-loader
      vulkan-validation-layers
      amdvlk
      mesa.opencl
    ];
  };
  hardware.amdgpu.amdvlk.enable = true;

  # Power
  powerManagement = {
    enable = true;
    powertop.enable = true;
  };

  # Swap
  swapDevices = [{
    device = "/var/lib/swap";
    size = 16 * 1024;
  }];
  boot.resumeDevice = "/var/lib/swap";

  # Display Manager
  services.displayManager.autoLogin.user = "priime";

  # Mouse
  services.libinput.mouse = { middleEmulation = false; };

  # Notification Server
  systemd.user.services.deadd-notification-center = {
    description = "deadd notification center";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig = {
      ExecStart =
        "${pkgs.deadd-notification-center}/bin/deadd-notification-center";
      Restart = "always";
      RestartSec = 3;
    };
  };

  virtualisation.docker.enable = true;

  users.users.priime = {
    isNormalUser = true;
    description = "Lucas";
    extraGroups = [ "networkmanager" "wheel" "docker" ];
    packages = with pkgs; [ ];
    shell = pkgs.fish;
  };

  documentation.dev.enable = true;

  environment.variables = {
    EDITOR = "emacsclient";
    CC = "gcc";
    LSP_USE_PLISTS = "true";
    MOZ_USE_XINPUT2 = "1";
    RUSTICL_ENABLE="radeonsi";
    ROC_ENABLE_PRE_VEGA = "1";
    QT_QPA_PLATFORM = "xcb";
  };

  ### Services

  # xserver
  services.xserver = {
    enable = true;
    displayManager.lightdm.enable = true;
    autoRepeatDelay = 350;
    autoRepeatInterval = 17;
  };

  # Window Manager
  programs.niri.enable = true;
  programs.xwayland.enable = true;

  services.tailscale = {
    enable = true;
    useRoutingFeatures = "client";
  };

  services.syncthing = {
    enable = true;
    user = "priime";
    dataDir = "/home/priime/syncthing";
    configDir = "/home/priime/.config/syncthing";
    overrideDevices = true;
    overrideFolders = false;
    settings = {
      gui = { user = "priime"; };
    };
  };

  # Programs
  programs.kdeconnect.enable = true;
  programs.steam.enable = true;
  programs.dconf.enable = true;
  programs.obs-studio.enable = true;

  programs.fish = {
    enable = true;
    shellAliases = {
      psax = "ps -ax | grep -i";
      ls = "eza";
      ll = "eza --all --long";
      e = "emacs";
      ed = "emacs --daemon";
      ec = ''emacsclient -n -c -a ""'';
    };
  };

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryPackage = lib.mkForce pkgs.pinentry-gtk2;
  };

  programs.msmtp = {
    enable = true;
    accounts = {
      default = {
        auth = true;
        host = "smtp.fastmail.com";
        passwordeval =
          "gpg --decrypt --no-tty -q ~/backupcodes/emacsframeworkfastmail.asc";
        user = "lucas@priime.dev";
      };
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?
  home-manager.users.priime = { pkgs, ... }: { imports = [ ./home.nix ]; };
}
