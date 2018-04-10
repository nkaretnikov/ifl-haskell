{ mkDerivation, base, HUnit, stdenv }:
mkDerivation {
  pname = "ifl-haskell";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base HUnit ];
  homepage = "https://github.com/nkaretnikov/ifl-haskell";
  description = "\"Implementing Functional Languages: a tutorial\" in Haskell";
  license = stdenv.lib.licenses.publicDomain;
}
