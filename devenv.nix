{ pkgs, lib, config, inputs, ... }:

{
  packages = with pkgs; [
    lazygit
  ];

  languages.zig.enable = true;
}
