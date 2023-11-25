{ mkDerivation
, aeson
, aeson-pretty
, base
, base-compat-batteries
, bytestring
, Cabal
, cabal-doctest
, containers
, cookie
, doctest
, generics-sop
, Glob
, hashable
, hspec
, hspec-discover
, http-media
, HUnit
, insert-ordered-containers
, lens
, lib
, mtl
, optics-core
, optics-th
, QuickCheck
, quickcheck-instances
, scientific
, template-haskell
, text
, time
, transformers
, unordered-containers
, utf8-string
, uuid-types
, vector
}:
mkDerivation {
  pname = "openapi3";
  version = "3.2.3";
  sha256 = "d4e4570955bcc66fde72c3070bc3ef4bcf54b82ef71448f6541256d79dfa736b";
  revision = "4";
  editedCabalFile = "1wpdmp3xp948052y325h64smp6l809r8mcvh220bfbrb4vrbk43b";
  isLibrary = true;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    aeson
    aeson-pretty
    base
    base-compat-batteries
    bytestring
    containers
    cookie
    generics-sop
    hashable
    http-media
    insert-ordered-containers
    lens
    mtl
    optics-core
    optics-th
    QuickCheck
    scientific
    template-haskell
    text
    time
    transformers
    unordered-containers
    uuid-types
    vector
  ];
  executableHaskellDepends = [ aeson base lens text ];
  testHaskellDepends = [
    aeson
    base
    base-compat-batteries
    bytestring
    containers
    doctest
    Glob
    hashable
    hspec
    HUnit
    insert-ordered-containers
    lens
    mtl
    QuickCheck
    quickcheck-instances
    template-haskell
    text
    time
    unordered-containers
    utf8-string
    vector
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/biocad/openapi3";
  description = "OpenAPI 3.0 data model";
  license = lib.licenses.bsd3;
  mainProgram = "example";
  doCheck = false;
}
