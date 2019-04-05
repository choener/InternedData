let
  bimaps = (import ../bimaps/package.nix);
  lib =
    { src = ./lib;
      dependencies = [
        "aeson"
        "binary"
        "bytestring"
        "cereal"
        "cereal-text"
        "cereal-vector"
        "containers"
        "deepseq"
        "hashable"
        "lens"
        "string-conversions"
        "text"
        "text-binary"
        "utf8-string"
        "vector"
        "vector-binary-instances"
        "vector-th-unbox"
        ] ++ bimaps.dependencies;
      extensions = [
        "BangPatterns"
        "CPP"
        "DataKinds"
        "DefaultSignatures"
        "DeriveDataTypeable"
        "DeriveFoldable"
        "DeriveFunctor"
        "DeriveGeneric"
        "DeriveTraversable"
        "FlexibleContexts"
        "FlexibleInstances"
        "FunctionalDependencies"
        "GADTs"
        "GeneralizedNewtypeDeriving"
        "MultiParamTypeClasses"
        "PatternGuards"
        "PatternSynonyms"
        "PolyKinds"
        "RankNTypes"
        "RecordWildCards"
        "ScopedTypeVariables"
        "StandaloneDeriving"
        "TemplateHaskell"
        "TypeApplications"
        "TypeFamilies"
        "TypeOperators"
        "UndecidableInstances"
        "UnicodeSyntax"
        ] ++ bimaps.extensions;
      # these are my (local) packages
      packages = [ bimaps ] ++ bimaps.packages;
    };
in
  lib

