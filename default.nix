{ mkDerivation, aeson, base, bimaps, binary, bytestring, cereal
, cereal-text, compact, containers, criterion, deepseq, hashable
, QuickCheck, stdenv, string-conversions, tasty, tasty-quickcheck
, tasty-th, text, text-binary, utf8-string, vector-th-unbox
}:
mkDerivation {
  pname = "InternedData";
  version = "0.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bimaps binary bytestring cereal cereal-text compact
    deepseq hashable string-conversions text text-binary utf8-string
    vector-th-unbox
  ];
  testHaskellDepends = [
    aeson base binary cereal QuickCheck string-conversions tasty
    tasty-quickcheck tasty-th
  ];
  benchmarkHaskellDepends = [
    base bytestring containers criterion deepseq text
  ];
  homepage = "https://github.com/choener/InternedData";
  description = "Data interning (with compact regions where possible)";
  license = stdenv.lib.licenses.bsd3;
}
