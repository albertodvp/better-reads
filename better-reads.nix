{ mkDerivation, base, cassava, fourmolu, hlint, hspec, http-types
, lib, lucid, mtl, optparse-applicative, QuickCheck
, quickcheck-instances, random, random-shuffle, relude, servant
, servant-lucid, servant-server, vector, wai, wai-extra, warp
}:
mkDerivation {
  pname = "better-reads";
  version = "0.0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base cassava fourmolu hlint http-types lucid mtl
    optparse-applicative random random-shuffle relude servant
    servant-lucid servant-server vector wai wai-extra warp
  ];
  executableHaskellDepends = [
    base cassava fourmolu hlint http-types lucid mtl
    optparse-applicative random random-shuffle relude servant
    servant-lucid servant-server vector wai wai-extra warp
  ];
  testHaskellDepends = [
    base cassava fourmolu hlint hspec http-types lucid mtl
    optparse-applicative QuickCheck quickcheck-instances random
    random-shuffle relude servant servant-lucid servant-server vector
    wai wai-extra warp
  ];
  homepage = "https://github.com/albertodvp/better-reads";
  description = "A toolkit to better handle goodread books";
  license = lib.licenses.mit;
}
