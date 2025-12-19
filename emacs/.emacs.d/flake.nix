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
        # Alternative: emacs-git for bleeding edge
        emacsPackage = pkgs.emacs-unstable.override {
          # Enable native compilation for speed
          withNativeCompilation = true;
          # Use Lucid for better rendering (optional)
          # toolkit = "lucid";
          # withGTK3 = false;
        };

        # Emacs with common dependencies
        emacsWithDeps = pkgs.emacsWithPackagesFromUsePackage {
          package = emacsPackage;
          config = ./init.el;
          defaultInitFile = false;
          alwaysEnsure = false;  # Using straight.el, not nix packages
        };

      in {
        packages = {
          default = emacsPackage;
          emacs = emacsPackage;
          emacs-with-deps = emacsWithDeps;
        };

        apps.default = flake-utils.lib.mkApp {
          drv = emacsPackage;
          name = "emacs";
        };

        # Development shell with Emacs and common dev tools
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            emacsPackage

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
          ];

          shellHook = ''
            echo "Emacs development environment loaded"
            echo "Emacs version: $(${emacsPackage}/bin/emacs --version | head -1)"
          '';
        };
      }
    ) // {
      # Home-manager module
      homeManagerModules.default = { config, lib, pkgs, ... }:
        let
          cfg = config.programs.morphEmacs;
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
              package = pkgs.emacs-unstable.override {
                withNativeCompilation = true;
              };
            };

            # Emacs daemon service
            services.emacs = {
              enable = true;
              client.enable = true;
              defaultEditor = true;
            };
          };
        };
    };
}
