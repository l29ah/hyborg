{ pkgs ? import nix/default-nixpkgs.nix {}
, src  ? ./.
}:
let
  hs = import nix/extra-hs-packages.nix { inherit pkgs; };
in
hs.callCabal2nix "hyborg" src {}
