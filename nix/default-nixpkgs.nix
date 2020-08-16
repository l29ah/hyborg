{ config ? null }:
let
  # Ref "nixpkgs-unstable", 14 August 2020.
  # Need newer GHC from unstable and newer Hackage dependencies.
  commit = "729e7295cf7b3205fcfa72544c1195c03de11c3d";

  nixpkgs-src = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${commit}.tar.gz";
    sha256 = "0mxhi0lc11aa3r7i7din1q2rjg5c4amq3alcr8ga2fcb64vn2zd3";
  };
in
import nixpkgs-src (if isNull config then {} else { inherit config; })
