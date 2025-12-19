{
  description = "Morph's Optimized Emacs Configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    # Emacs overlay for latest Emacs builds
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, emacs-overlay, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ emacs-overlay.overlay ];
          config.allowUnfree = true;
        };

        # Use emacs-unstable with native compilation for best performance
        emacsPackage = pkgs.emacs-unstable.override {
          withNativeCompilation = true;
        };

        # Define all packages to install
        emacsWithPackages = (pkgs.emacsPackagesFor emacsPackage).emacsWithPackages (epkgs: with epkgs; [
          # Core
          use-package
          gcmh

          # Evil ecosystem
          evil
          evil-collection
          evil-org
          evil-commentary
          undo-tree

          # Completion
          counsel
          ivy
          ivy-rich
          swiper
          flx
          smex
          vertico

          # Git
          magit
          magit-delta
          magit-popup
          git-commit
          magit-section
          with-editor

          # Org ecosystem
          org-roam
          org-roam-ui
          org-msg

          # Terminal
          vterm
          multi-vterm
          eat

          # UI/UX
          which-key
          rainbow-delimiters
          olivetti
          deadgrep
          circadian
          autothemer
          gruvbox-theme
          modus-themes

          # Dired
          dired-hide-dotfiles
          nerd-icons-dired
          nerd-icons
          async

          # Editing
          yasnippet
          markdown-mode
          nix-mode
          slime
          pdf-tools

          # Utilities
          exec-path-from-shell
          atomic-chrome
        ]);

      in {
        packages = {
          default = emacsWithPackages;
          emacs = emacsPackage;
          emacs-with-packages = emacsWithPackages;
        };

        apps.default = flake-utils.lib.mkApp {
          drv = emacsWithPackages;
          name = "emacs";
        };

        # Development shell with Emacs and common dev tools
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            emacsWithPackages

            # LSP servers
            nodePackages.typescript-language-server
            nodePackages.vscode-langservers-extracted
            pyright
            rust-analyzer
            gopls
            nil  # Nix LSP

            # Tools
            ripgrep
            fd
            git
            delta  # For magit-delta

            # Build dependencies for native compilation
            gcc
            libgccjit

            # Emacs dependencies
            sqlite  # For org-roam
            mu
            isync   # For mu4e mail sync

            # vterm dependencies
            cmake
            libtool
          ];

          shellHook = ''
            echo "Emacs development environment loaded"
            echo "Emacs version: $(${emacsWithPackages}/bin/emacs --version | head -1)"
          '';
        };
      }
    ) // {
      # Home-manager module
      homeManagerModules.default = { config, lib, pkgs, ... }:
        let
          cfg = config.programs.morphEmacs;
          isDarwin = pkgs.stdenv.isDarwin;
          emacsPkgs = pkgs.emacsPackagesFor (pkgs.emacs-unstable.override {
            withNativeCompilation = true;
          });
        in {
          options.programs.morphEmacs = {
            enable = lib.mkEnableOption "Morph's optimized Emacs configuration";
          };

          config = lib.mkIf cfg.enable {
            home.file.".emacs.d" = {
              source = ./.;
              recursive = true;
            };

            programs.emacs = {
              enable = true;
              package = emacsPkgs.emacsWithPackages (epkgs: with epkgs; [
                use-package gcmh
                evil evil-collection evil-org evil-commentary undo-tree
                counsel ivy ivy-rich swiper flx smex vertico
                magit magit-delta magit-popup git-commit magit-section with-editor
                org-roam org-roam-ui org-msg
                vterm multi-vterm eat
                which-key rainbow-delimiters olivetti deadgrep
                circadian autothemer gruvbox-theme modus-themes
                dired-hide-dotfiles nerd-icons-dired nerd-icons async
                yasnippet markdown-mode nix-mode slime pdf-tools
                exec-path-from-shell atomic-chrome
              ]);
            };

            # Emacs daemon service (Linux only - not available on Darwin)
            services.emacs = lib.mkIf (!isDarwin) {
              enable = true;
              client.enable = true;
              defaultEditor = true;
            };
          };
        };
    };
}
