{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "text-class"; version = "2019.10.16"; };
      license = "Apache-2.0";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-wallet";
      url = "";
      synopsis = "Extra helpers to convert data-types to and from Text";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.casing)
          (hsPkgs.extra)
          (hsPkgs.fmt)
          (hsPkgs.text)
          (hsPkgs.hspec)
          (hsPkgs.QuickCheck)
          ];
        };
      tests = {
        "unit" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.hspec)
            (hsPkgs.QuickCheck)
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
    postUnpack = "sourceRoot+=/lib/text-class; echo source root reset to \$sourceRoot";
    }