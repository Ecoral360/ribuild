# Changelog

All notable changes to Ribuild are documented in this file.

## 2026-09-30

### Added

- `rib build -t <TARGET>` and `rib build -s <SCRIPT> -t <TARGET>` only build
  the given target, instead of every target of the package.

### Changed

- `rib run` (and `rib run -s`, `rib test`) now only builds the target it runs
  (the one given with `-t`, or the first target of the package), instead of
  building every target before running.
- `rib --version` now prints `Ribuild v<VERSION>` instead of only the version.
