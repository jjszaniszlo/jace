{ pkgs, lib, config, inputs, ... }:

{
  packages = [ pkgs.git pkgs.lazygit ];

  languages.rust.enable = true;
}
