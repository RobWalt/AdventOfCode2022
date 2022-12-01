{ mkDerivation, base, lib, text }:
mkDerivation {
  pname = "day1";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base text ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
