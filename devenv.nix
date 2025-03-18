{ pkgs, lib, config, inputs, ... }:

{
  languages.ocaml.enable = true;

  packages = with pkgs; [
    lazygit
    ocamlPackages.ounit2
    ocamlPackages.fmt
    ocamlPackages.ocaml-lsp
  ];
}
