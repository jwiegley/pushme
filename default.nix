{ mkDerivation, aeson, base, bytestring, pipes, pipes-group, pipes-safe
, pipes-text, containers, io-storage, lens, logging, monad-logger
, old-locale, optparse-applicative, parallel-io, regex-posix, safe
, shelly, stdenv, system-fileio, system-filepath, temporary, text
, text-format, time, transformers, unix, unordered-containers, yaml
, foldl
}:
mkDerivation {
  pname = "pushme";
  version = "2.0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    aeson base bytestring pipes pipes-group pipes-safe pipes-text containers
    io-storage lens logging monad-logger old-locale optparse-applicative
    parallel-io regex-posix safe shelly system-fileio system-filepath
    temporary text text-format time transformers unix unordered-containers
    yaml foldl
  ];
  homepage = "https://github.com/jwiegley/pushme";
  description = "Tool to synchronize directories with rsync, zfs or git-annex";
  license = stdenv.lib.licenses.bsd3;
}
