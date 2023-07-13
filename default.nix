{ mkDerivation, aeson, base, bimaps, binary, bytestring, cereal
, cereal-text, compact, containers, criterion, deepseq, hashable
, lens, lib, QuickCheck, string-conversions, tasty
, tasty-quickcheck, tasty-th, text, text-binary, utf8-string
, vector-th-unbox
}:
mkDerivation {
  pname = "InternedData";
  version = "0.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bimaps binary bytestring cereal cereal-text compact
    deepseq hashable lens string-conversions text text-binary
    utf8-string vector-th-unbox
  ];
  testHaskellDepends = [
    aeson base bimaps binary bytestring cereal cereal-text compact
    deepseq hashable lens QuickCheck string-conversions tasty
    tasty-quickcheck tasty-th text text-binary utf8-string
    vector-th-unbox
  ];
  benchmarkHaskellDepends = [
    aeson base bimaps binary bytestring cereal cereal-text compact
    containers criterion deepseq hashable lens string-conversions text
    text-binary utf8-string vector-th-unbox
  ];
  homepage = "https://github.com/choener/InternedData";
  description = "Data interning (with compact regions where possible)";
  license = lib.licenses.bsd3;
}
