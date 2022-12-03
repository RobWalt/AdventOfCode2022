{ mkDerivation, base, containers, lib, split, text }:
mkDerivation {
  pname = "day3";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers split text ];
  license = lib.licenses.agpl3Plus;
  mainProgram = "day3";
}
