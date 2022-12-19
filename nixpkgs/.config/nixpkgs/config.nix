{
  allowUnfree = true;
  allowUnsupportedSystem = true;
}
# https://stackoverflow.com/questions/45417479/how-can-i-use-a-nix-configuration-when-not-using-nixos
# let
#     pkgs = import <nixpkgs> {};
# in
#     {    
#         packages = [
#             pkgs.vim
#             pkgs.gimp
#         ];
#     }
