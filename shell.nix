{ pkgs ? import nix/default-nixpkgs.nix {}
}:
let
  hyborg = import ./. { inherit pkgs; };
in
pkgs.mkShell {
  buildInputs = [
    (pkgs.haskell.lib.justStaticExecutables hyborg)
  ];
}
