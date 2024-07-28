{ inputs, config, lib, pkgs, ... }: {
  nixpkgs.config.allowUnfree = true;

  nix = {
    extraOptions = ''
      extra-experimental-features = nix-command flakes
      build-users-group = nixbld
    '';
    settings = {
      trusted-users = [ "root" "priime" ];
      substituters =
        [ "https://cache.nixos.org/" "https://nix-community.cachix.org" ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
      sandbox = true;
    };
    nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
  };

  environment.systemPackages = with pkgs;
    let
      packages = [
        nixos-rebuild
        coreutils
        git
        gnupg
        bottom
        htop
        bat
        jq
        unzip
        wget
        curl
        libtool
        fish
        gcc
        gnumake
        cmake
        tldr
        emacs
        man-pages
        gdb
        zsh
        alsa-utils
        killall
        feh
        xcolor
        xclip
        xdotool
        ripgrep
        fd
        home-manager

        nix-index
        nixfmt

        rustc
        cargo
        rustfmt
        clippy
        rust-analyzer

        python3

        nodejs
        yarn

        racket

        jdk8
        jdk21
        jdk22

        nodejs_20
      ];
    in packages;
}
