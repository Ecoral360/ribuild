# Changelog

All notable changes to Ribuild are documented in this file.

## 2026-09-30

### Added

- `rib install` (or `rib i`) installs the `dependencies` of the package in its
  `dependency-dir` (default: `lib`), each one in a directory named after the
  dependency. Supported sources: `(github "owner/repo")` and `(git "<url>")`.
- A symbol in `includes` includes the installed dependency with that name:
  everything its `package.scm` includes (its own dependencies included) is
  included, with the paths made relative to the dependency directory.
- `rib build -t <TARGET>` and `rib build -s <SCRIPT> -t <TARGET>` only build
  the given target, instead of every target of the package.

### Changed

- `rib run` (and `rib run -s`, `rib test`) now only builds the target it runs
  (the one given with `-t`, or the first target of the package), instead of
  building every target before running.
- `rib --version` now prints `Ribuild v<VERSION>` instead of only the version.
