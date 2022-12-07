{ mkDerivation, attoparsec, base, containers, lib, text }:
mkDerivation {
  pname = "day7";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ attoparsec base containers text ];
  license = lib.licenses.agpl3Plus;
  mainProgram = "day7";
}
