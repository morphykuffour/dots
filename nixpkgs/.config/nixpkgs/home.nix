{ config, current, pkgs, lib, ... }:

# let
# userConfig = import ./user.nix { };
# gitConfig = import ./modules/git.nix {};
# in
# ${builtins.getEnv "HOME"}
# with ; {
let
  tex = (pkgs.texlive.combine {
    inherit (pkgs.texlive) scheme-full
      dvisvgm dvipng # for preview and export as html
      xcolor wrapfig amsmath ulem hyperref capt-of;
      #(setq org-latex-compiler "lualatex")
      #(setq org-preview-latex-default-process 'dvisvgm)
  });
in
{
  imports =
    [
      # ./modules/tmux/tmux.nix
      # ./modules/shell.nix
      ./modules/i3.nix
      ./modules/spotify.nix
      ./modules/redshift.nix
      ./modules/pass.nix
      ./modules/fonts.nix
      ./modules/sxhkd.nix
      # ./modules/nvim.nix
      ./modules/rofi.nix
      ./modules/himalaya.nix
      # ./modules/nur.nix
    ];


  services.clipmenu.enable = true;
  programs = {
    home-manager = {
      enable = true;
    };
    obs-studio = {
      enable = true;
      plugins = with pkgs.obs-studio-plugins; [
        wlrobs
        obs-gstreamer
      ];
    };
  };
  nixpkgs.config.allowUnfree = true;

# nixpkgs.overlays = [(self: super: { discord = super.discord.overrideAttrs (_: { src = builtins.fetchTarball <link-to-tarball>; });})];

  home = {
    username = "morp";
    homeDirectory = "/home/morp";
    stateVersion = "22.05";

    packages = with pkgs;[

      # devtools
      kitty 
      tmux
      zsh
      atuin 
      starship 
      exa 
      bat
      tealdeer 
      fd 
      gh
      clipmenu
      delta 
      cscope
      pastel 
      conda 
      jupyter 
      ruby 
      edir 
      ranger 
      stylua 
      cargo
      jq 
      curl 
      fzf 
      nodejs 
      sbcl


      # mail
      neomutt # email client
      mu
      isync # sync mail locally
      msmtp # send mail
      pass # encrypt passwords
      himalaya
      offlineimap    

      # productivity
      calibre
      emacs 
      slides 
      uxplay

      # windows
      sxhkd
      tex

      # creativity
      davinci-resolve
      inkscape
      gimp
      blender
      kicad
      ffmpeg
      eva
      aria2
      hyperfine 
      hexyl 
      ripgrep 
      autojump 
      pandoc
      croc
      spotify
      neofetch
      zathura
      go
      viu
      mpv
      feh
      sublime
      surfraw
      nix-index
      vial
      redshift
      termite
      discord

      # neovim
      neovim-unwrapped
      tree-sitter
      rnix-lsp
      gopls
      ccls
      tree-sitter-grammars.tree-sitter-markdown
      sumneko-lua-language-server
      nodePackages.typescript-language-server
      # nodePackages.insect
      nodePackages.bash-language-server
      nodePackages.pyright
      nodePackages.typescript
      nodePackages.prettier
      black
      rust-analyzer

      (python39.withPackages (pp: with pp; [
        pynvim
        pandas
        requests
        pip
        ipython
        dbus-python
        html2text
        icalendar
        keymapviz
        virtualenv

      ]))
    ];
  };
}

