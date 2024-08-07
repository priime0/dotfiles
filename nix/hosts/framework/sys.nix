{ inputs, config, pkgs, lib, ... }: {
  imports = [ ./hw.nix ];

  networking.hostName = "framework";
  time.timeZone = "America/Los_Angeles";

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
  hardware.pulseaudio.enable = false;
  services.pipewire = {
    enable = true;
    pulse.enable = true;
  };

  # AMD
  hardware.cpu.amd.updateMicrocode = true;
  hardware.graphics.enable = true;

  # Power
  powerManagement = {
    enable = true;
    powertop.enable = true;
  };

  # Autologin
  services.displayManager.autoLogin.user = "priime";

  # Mouse
  services.libinput.mouse = { middleEmulation = false; };

  users.users.priime = {
    isNormalUser = true;
    description = "Lucas";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [ ];
  };

  documentation.dev.enable = true;

  environment.variables = {
    EDITOR = "emacsclient";
    CC = "gcc";
    LSP_USE_PLISTS = "true";
    MOZ_USE_XINPUT2 = "1";
  };

  ### Services

  # xserver and i3
  services.xserver = {
    enable = true;
    windowManager.i3.enable = true;
    displayManager.lightdm.enable = true;
    xkb.layout = "us";
    xkb.variant = "dvp";
    autoRepeatDelay = 350;
    autoRepeatInterval = 17;
  };

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
      devices = { };
      folders = { };
      gui = { user = "priime"; };
    };
  };

  # Programs
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryPackage = lib.mkForce pkgs.pinentry-gtk2;
  };
  programs.kdeconnect.enable = true;
  programs.steam.enable = true;
  programs.dconf.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?
  home-manager.users.priime = { pkgs, ... }: { imports = [ ./home.nix ]; };
}
