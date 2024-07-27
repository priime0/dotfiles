{ inputs, configDir, config, pkgs, lib, ... }: {
  # Extra $PATH directories
  home.sessionPath = [ ];

  nixpkgs.config.allowUnfree = true;

  programs.git = {
    enable = true;
    userName = "Lucas Sta Maria";
    userEmail = "lucas@priime.dev";
    signing = {
      signByDefault = true;
      key = "2EAF5BA156A36810B057BF61F07FB16A826E3B47";
    };
    extraConfig = { credential.helper = "store"; };
  };
}
