{ mkDerivation, base, hasktags, HUnit, stdenv }:
mkDerivation {
  pname = "ifl-haskell";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base hasktags ];
  executableHaskellDepends = [ base hasktags HUnit ];
  homepage = "https://github.com/nkaretnikov/ifl-haskell";
  description = "\"Implementing Functional Languages: a tutorial\" in Haskell";
  license = stdenv.lib.licenses.publicDomain;
}
