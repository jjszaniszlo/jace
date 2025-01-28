{ pkgs, lib, config, inputs, ... }:
let
  nixvim = inputs.nixvim.packages.${pkgs.system}.default;
in
{
  packages = [ pkgs.git pkgs.lazygit nixvim];

  languages.rust.enable = true;
  languages.lua.enable = true;
  languages.lua.package = pkgs.luajit;
}
