{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-wallet-cli"; version = "2019.10.16"; };
      license = "Apache-2.0";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-wallet";
      url = "";
      synopsis = "Utilities for a building Command-Line Interfaces";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.aeson)
          (hsPkgs.aeson-pretty)
          (hsPkgs.ansi-terminal)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-wallet-core)
          (hsPkgs.directory)
          (hsPkgs.extra)
          (hsPkgs.filepath)
          (hsPkgs.fmt)
          (hsPkgs.http-client)
          (hsPkgs.iohk-monitoring)
          (hsPkgs.servant-client)
          (hsPkgs.servant-client-core)
          (hsPkgs.servant-server)
          (hsPkgs.text)
          (hsPkgs.text-class)
          (hsPkgs.optparse-applicative)
          ];
        };
      tests = {
        "unit" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-crypto)
            (hsPkgs.cardano-wallet-cli)
            (hsPkgs.cardano-wallet-core)
            (hsPkgs.filepath)
            (hsPkgs.hspec)
            (hsPkgs.memory)
            (hsPkgs.optparse-applicative)
            (hsPkgs.QuickCheck)
            (hsPkgs.temporary)
            (hsPkgs.text)
            (hsPkgs.text-class)
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover or (pkgs.buildPackages.hspec-discover))
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-wallet.git";
      rev = "52eb3191604ea607ca04f287bc6efe6407a2241b";
      sha256 = "1s8wh7zcpg5bfy24vwnil325bphr7ccsl5f17jjhmsw0g6a1ijgq";
      });
    postUnpack = "sourceRoot+=/lib/cli; echo source root reset to \$sourceRoot";
    }