{ mkDerivation, base, lib, matrix, text, vector }:
mkDerivation {
  pname = "day8";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base matrix text vector ];
  license = lib.licenses.agpl3Plus;
  mainProgram = "day8";
}
