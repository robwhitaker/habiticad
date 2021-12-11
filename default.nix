{ mkDerivation, aeson, base, dbus, directory, exceptions
, fdo-notify, filepath, lib, process, relude, req, text
}:
mkDerivation {
  pname = "habiticad";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    aeson base dbus directory exceptions fdo-notify filepath process
    relude req text
  ];
  description = "Daemon that listens for dbus events and launches Habitica requests";
  license = lib.licenses.bsd3;
}
