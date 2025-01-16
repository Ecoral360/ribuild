# Ribuild

A package manager for ribbit

## Installation

The installation of `ribuild` will install the compatible ribbit compiler (`rsc`).
To ensure a proper installation, _do not install the ribbit compiler yourself_
and let `ribuild` do it (the installation step will make `rsc` accessible,
so you will be able to use the stand alone compiler after installing `ribuild`).

1. Install gambit
2. Run
   ```sh
   git clone https://github.com/Ecoral360/ribuild.git
   cd ribuild
   make install
   ```
3. Run the command provided at the end of the installation phase to add `rib`
   to your PATH (this will also add `rsc` to your PATH)

4. Run `rib --version` to ensure the installation was successful

## Features (WIP)

```sh
rb init '<NAME>' [-d {destination=.}]
# creates a ribconf.scm file in the destination (default .)
# NAME is the name of the package being created by ribuild

rb run [-t target]

rb build [-t target]
# builds the code in the specified target (or all the target if -t is absent)

rb pack
# produces a zip file ribbit package

rb install '<PACKAGE>'

rb test [-t target]

rb test-all-eq '[--input|-i <INPUT-FILE|->]' '[--before <FILE.scm>]' '[--include <target1,target2,targetn>]' '[--exclude <target1,target2,targetn>]'
# tests that all targets produce the same output from the same input
```
