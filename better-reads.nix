{ mkDerivation, base, cassava, hspec, http-types, lib, lucid, mtl
, openapi3, optparse-applicative, QuickCheck, quickcheck-instances
, random, random-shuffle, relude, servant, servant-lucid
, servant-openapi3, servant-server, vector, wai, wai-extra, warp
}:
mkDerivation {
  pname = "better-reads";
  version = "0.0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base cassava http-types lucid mtl openapi3 optparse-applicative
    random random-shuffle relude servant servant-lucid servant-openapi3
    servant-server vector wai wai-extra warp
  ];
  executableHaskellDepends = [
    base cassava http-types lucid mtl openapi3 optparse-applicative
    random random-shuffle relude servant servant-lucid servant-openapi3
    servant-server vector wai wai-extra warp
  ];
  testHaskellDepends = [
    base cassava hspec http-types lucid mtl openapi3
    optparse-applicative QuickCheck quickcheck-instances random
    random-shuffle relude servant servant-lucid servant-openapi3
    servant-server vector wai wai-extra warp
  ];
  homepage = "https://github.com/albertodvp/better-reads";
  description = "A toolkit to better handle goodread books";
  license = lib.licenses.mit;
}
