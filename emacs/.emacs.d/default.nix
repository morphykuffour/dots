# source: https://raw.githubusercontent.com/adisbladis/nixconfig/5df69fb32f91ad62743d21aa6d654e98248ea40f/modules/emacs/default.nix
{
  config,
  pkgs,
  lib,
  ...
}: let
  emacsGit =
    pkgs.callPackage
    (
      {emacsWithPackagesFromUsePackage}: (emacsWithPackagesFromUsePackage {
        # package = pkgs.emacsNativeComp.override {
        #   toolkit = "lucid";
        #   withGTK3 = false;
        #   withXinput2 = true;
        # };
        package = emacsGit;
        config = ./init.el;
        alwaysEnsure = true;

        override = epkgs:
          epkgs
          // {
            tree-sitter-langs = epkgs.tree-sitter-langs.withPlugins (
              # Install all tree sitter grammars available from nixpkgs
              grammars: builtins.filter lib.isDerivation (lib.attrValues grammars)
            );
          };
      })
    )
    {};

  cfg = config.my.emacs;
in {
  options.my.emacs.enable = lib.mkEnableOption "Enable Emacs.";

  config = lib.mkIf cfg.enable {
    # home-manager.users.morp = {...}: {
    #   home.file.".emacs".source = ./emacs.el;
    # };

    home-manager.users.morp = { ... }: {
      services.emacs = {
        enable = true;
        client.enable = true;
        socketActivation.enable = true;
        package = emacsGit;
      };
      home = {
        packages = [
          (pkgs.writeShellScriptBin "emacs" ''${config.my.emacs.package}/bin/emacsclient -c "$@"'')
          (pkgs.writeShellScriptBin "emacsclient" ''${config.my.emacs.package}/bin/emacsclient "$@"'')
        ];
        # sessionVariables = { EDITOR = "emacs"; };
      };
      xdg.configFile = {
        "emacs/init.el".text = config.chvp.base.emacs.fullConfig;
        "emacs/early-init.el".source = ./early-init.el;
      };
    };

    # environment.systemPackages = [
    environment = {
      systemPackages = with pkgs; [
        emacsGit

        nixpkgs-fmt
        nodePackages.vscode-langservers-extracted

        ccls
        nodePackages.bash-language-server
        nodePackages.typescript
        nodePackages.typescript-language-server
        pyright
        rnix-lsp
        gopls
        rust-analyzer
      ];
    };
  };
}
