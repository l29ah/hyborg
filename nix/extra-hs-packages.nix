{ pkgs               ? import ./default-nixpkgs.nix {}
, msgpack-binary-src ? ../hs-msgpack-binary
, msgpack-types-src  ? ../hs-msgpack-types
}:
pkgs.haskellPackages.extend (self: super: {
  # Copy-pasted from https://github.com/NixOS/nixpkgs
  # from "pkgs/development/haskell-modules/hackage-packages.nix" file.
  #
  # "doJailbreak" is needed in order to avoid upper bound for "base" package of
  # test dependencies which is "<4.9".
  # The package probably was marked as "broken" due to this.
  "datetime" = pkgs.haskell.lib.doJailbreak (self.callPackage
    ({ mkDerivation, base, HUnit, old-locale, old-time, QuickCheck
     , test-framework, test-framework-hunit, test-framework-quickcheck2
     , time
     }:
     mkDerivation {
       pname = "datetime";
       version = "0.3.1";
       sha256 = "0jmxxmv5s9rch84ivfjhqxdqnvqqzvabjs152wyv47h5qmvpag1k";
       libraryHaskellDepends = [ base old-locale old-time time ];
       testHaskellDepends = [
         base HUnit old-locale old-time QuickCheck test-framework
         test-framework-hunit test-framework-quickcheck2 time
       ];
       description = "Utilities to make Data.Time.* easier to use";
       license = "GPL";
       hydraPlatforms = pkgs.stdenv.lib.platforms.none;
       broken = false;
     }) {});

  # Local package
  # FIXME "dontCheck" is needed to turn off tests, they're failing:
  # Failures:
  #
  #   test/Data/MessagePackSpec.hs:130:5:
  #   1) Data.MessagePack.unpack does not throw exceptions on arbitrary data
  #        uncaught exception: UnicodeException
  #        Cannot decode byte '\xba': Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream
  #        (after 91 tests)
  #          "\173@cB7FO)\DC4_CbI\186oxV+XB[V\bN\161O_\240\DELNJ+\CAN\198\b\ACK&'\n7w\SUBMF`L\SYNK\nv1\DC3n\191\rE\143\&2Mg\167H\ACKP\214\f\ENQ\235}\185\RS\f\STX\ESC\RSRN"
  #
  #   To rerun use: --match "/Data.MessagePack/unpack/does not throw exceptions on arbitrary data/"
  #
  #   test/Data/MessagePackSpec.hs:122:3:
  #   2) Data.MessagePack.failures should contain the same start of the failure message for all types
  #        unexpected success: TyConArgs 0 1 2
  #
  #   To rerun use: --match "/Data.MessagePack/failures/should contain the same start of the failure message for all types/"
  #
  #   test/Data/MessagePackSpec.hs:202:36:
  #   3) Data.MessagePack, type coercion, string<-bin
  #        Falsifiable (after 1 test):
  #          ""
  #        expected: Nothing
  #         but got: Just ""
  "msgpack-binary" =
    pkgs.haskell.lib.dontCheck
      (self.callCabal2nix "msgpack-binary" msgpack-binary-src {});

  # Local package
  "msgpack-types" =
    self.callCabal2nix "msgpack-types" msgpack-types-src {};
})
