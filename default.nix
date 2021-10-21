{ mkDerivation, base, containers, lib, record-dot-preprocessor
, record-hasfield, relude, text
}:
mkDerivation {
  pname = "habiticad";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers record-dot-preprocessor record-hasfield relude text
  ];
  description = "Daemon that listens for dbus events and launches Habitica requests";
  license = lib.licenses.bsd3;
}
