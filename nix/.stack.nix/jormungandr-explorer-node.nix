{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "jormungandr-explorer-node"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "(c) 2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-explorer";
      url = "";
      synopsis = "The DB node for the Jormungandr Block Explorer";
      description = "A Cardano node that follows the Jormungandr chain and inserts data from the\nchain into a PostgresQL database.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.base16-bytestring)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-config)
          (hsPkgs.cardano-explorer-db)
          (hsPkgs.cardano-node)
          (hsPkgs.cardano-prelude)
          (hsPkgs.cardano-shell)
          (hsPkgs.cardano-wallet-jormungandr)
          (hsPkgs.cborg)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.extra)
          (hsPkgs.formatting)
          (hsPkgs.io-sim-classes)
          (hsPkgs.iohk-monitoring)
          (hsPkgs.memory)
          (hsPkgs.monad-logger)
          (hsPkgs.optparse-applicative)
          (hsPkgs.network)
          (hsPkgs.persistent)
          (hsPkgs.prometheus)
          (hsPkgs.reflection)
          (hsPkgs.serialise)
          (hsPkgs.stm)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.transformers)
          ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././jormungandr-explorer-node; }