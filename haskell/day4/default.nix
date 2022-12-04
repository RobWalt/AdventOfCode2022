{ mkDerivation, base, lib }:
mkDerivation {
  pname = "day4";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  license = lib.licenses.agpl3Plus;
  mainProgram = "day4";
}
