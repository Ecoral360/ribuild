# Ribuild

A package manager for ribbit

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
