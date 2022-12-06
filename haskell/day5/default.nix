{ mkDerivation, attoparsec, base, lib, text }:
mkDerivation {
  pname = "day5";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ attoparsec base text ];
  license = lib.licenses.agpl3Plus;
  mainProgram = "day5";
}
