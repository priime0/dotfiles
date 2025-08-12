{ inputs, configDir, config, pkgs, lib, ... }: {
  home.stateVersion = "23.11";

  home.packages = let
    packages = with pkgs; [
      fish
      lsof
      rsync
      git
      curl
      wget

      tailscale

      nginx
    ];
  in packages;
}
