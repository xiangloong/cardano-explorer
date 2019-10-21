{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = {
        name = "cardano-wallet-launcher";
        version = "2019.10.16";
        };
      license = "Apache-2.0";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-wallet";
      url = "";
      synopsis = "Utilities for a building commands launcher";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.async)
          (hsPkgs.contra-tracer)
          (hsPkgs.fmt)
          (hsPkgs.iohk-monitoring)
          (hsPkgs.process)
          (hsPkgs.text)
          ] ++ (if system.isWindows
          then [ (hsPkgs.Win32) ]
          else [ (hsPkgs.unix) ]);
        };
      tests = {
        "unit" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-wallet-launcher)
            (hsPkgs.fmt)
            (hsPkgs.hspec)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.text)
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
    postUnpack = "sourceRoot+=/lib/launcher; echo source root reset to \$sourceRoot";
    }