{ pkgs, lib, config, inputs, ... }:
{
  packages = [ pkgs.git pkgs.lazygit];

  languages.rust.enable = true;
  languages.lua.enable = true;
  languages.lua.package = pkgs.luajit;
}
