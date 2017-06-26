{ mkDerivation, aeson, base, bytestring, containers, foldl
, io-storage, lens, logging, monad-logger, old-locale
, optparse-applicative, parallel-io, pipes, pipes-group, pipes-safe
, pipes-text, regex-posix, safe, shelly, stdenv, system-fileio
, system-filepath, temporary, text, text-format, time, transformers
, unix, unordered-containers, yaml
}:
mkDerivation {
  pname = "pushme";
  version = "2.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers foldl io-storage lens logging
    monad-logger old-locale optparse-applicative parallel-io pipes
    pipes-group pipes-safe pipes-text regex-posix safe shelly
    system-fileio system-filepath temporary text text-format time
    transformers unix unordered-containers yaml
  ];
  homepage = "https://github.com/jwiegley/pushme#readme";
  description = "Tool to synchronize directories with rsync, zfs or git-annex";
  license = stdenv.lib.licenses.bsd3;
}
