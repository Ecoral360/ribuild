# Ribuild 🐸

Ribuild (`rib`) is a build tool and package manager for
[Ribbit](https://github.com/Ecoral360/ribbit) Scheme projects.

You describe your project once in a `package.scm` file (sources, libraries,
features, targets), and `rib` drives the Ribbit compiler (`rsc`) to build it
for every target host (JavaScript, Python, ...). It also supports single-file
Scheme scripts that carry their own build configuration.

> **Status:** work in progress. The package format and commands may still
> change.

## Table of contents

- [Requirements](#requirements)
- [Installation](#installation)
- [Quick start](#quick-start)
- [Commands](#commands)
- [The `package.scm` file](#the-packagescm-file)
- [Includes and glob patterns](#includes-and-glob-patterns)
- [How a build works](#how-a-build-works)
- [Scripts](#scripts)
- [Testing](#testing)
- [Project layout](#project-layout)
- [Building Ribuild itself](#building-ribuild-itself)
- [License](#license)

## Requirements

- [Gambit Scheme](https://gambitscheme.org/) (v4.9.4+), used to build the
  Ribbit compiler
- The [Ribbit fork](https://github.com/Ecoral360/ribbit) used by Ribuild (make sure `rsc` is set to this version)
- [Node.js](https://nodejs.org/), since `rib` itself is compiled to JavaScript
- The runtime of every target host you build for (e.g. `python3` for a `py`
  target)

## Installation

1. Clone Ribuild:

   ```sh
   git clone https://github.com/Ecoral360/ribuild.git
   ```

2. Add `ribuild/bin` to your `PATH` (this provides `rib`):

   ```sh
   echo "export PATH=\"\$PATH:$(pwd)/bin\"" >> ~/.bashrc && . ~/.bashrc
   ```

3. Build `rib` (see [Building Ribuild itself](#building-ribuild-itself)):

   ```sh
   make
   ```

5. Check the installation:

   ```sh
   rib --version
   ```

**Keep the repository where it is:** `rib` finds its project templates
relative to the Ribuild sources it was compiled from. If you move the
repository, run `make` again.

## Quick start

```sh
rib init hello      # creates the hello/ package
cd hello
rib build           # compiles every target into build/
rib run             # builds, then runs the first target with an `exe`
```

`rib init hello` creates this layout:

```
hello/
|-- package.scm     package description
|-- src/
|   `-- main.scm    (define (main) (display "Hello from Ribuild!\n"))
`-- build/          target outputs
```

## Commands

```
rib <CMD> [OPTION]...
```

| Command                    | Description                                                       |
| -------------------------- | ----------------------------------------------------------------- |
| `rib init <NAME>`          | Creates the `<NAME>/` package from the default template           |
| `rib b`, `rib build`       | Builds every target of the package                                |
| `rib r`, `rib run`         | Builds every target, then runs the first target that has an `exe` |
| `rib test`                 | Like `run`, with the test features enabled (see [Testing](#testing)) |
| `rib init -s <FILE>`       | Adds a script configuration to `<FILE>` (see [Scripts](#scripts)) |
| `rib build -s <FILE>`      | Builds a script                                                   |
| `rib run -s <FILE>`        | Builds and runs a script                                          |
| `rib test -s <FILE>`       | Tests a script                                                    |
| `rib -v`, `rib --version`  | Prints the Ribuild version                                        |
| `rib -h`, `rib --help`     | Prints the usage                                                  |

### Options

| Option                          | Description                                                  |
| ------------------------------- | ------------------------------------------------------------ |
| `-t`, `--target <NAME>`         | `run`/`test`: run this target instead of the first `exe` one  |
| `-o`, `--output <FILE>`         | Write the compiled program to `<FILE>`                        |
| `-x`, `--exe <FILE>`            | Write the executable to `<FILE>`                              |
| `-q`, `--quiet`                 | Do not print the `[COMPILING]` / `[DONE]` messages            |
| `-k`, `--keep`                  | Keep the generated entry file after the compilation (see [How a build works](#how-a-build-works)) |
| `--target-output-suffix <SUF>`  | Append `<SUF>` to the output file name                        |
| `--target-exe-suffix <SUF>`     | Append `<SUF>` to the executable name (defaults to the output suffix) |
| `-- <ARGS>...`                  | `run`/`test`: everything after `--` is passed to the program  |

Example:

```sh
rib run -t js -- arg1 arg2
```

## The `package.scm` file

A package is described by a single `define-package` form:

```scheme
(define-package
  ; don't change the ribuild-version yourself, automatically set by ribuild
  (ribuild-version "1")

  (name "hello")
  (description "Add your description here !")
  (version "0.1.0")
  (authors ("John Doe"))

  (entry main)          ; procedure called to start the program
  (output-dir "build")  ; where the targets are written

  (includes             ; files and libraries that make up the program
    (ribbit "r4rs")
    "src/**")

  (features             ; compiler features, for all the targets
    +prim-no-arity      ; prefix `+` sets the feature to #t
    +v-port
    -js/web)            ; prefix `-` sets the feature to #f

  (targets
    (target "c"
      (output "hello.c")
      (exe "hello.c.exe"))

    (target "js"
      (includes "js/**")  ; only included when compiling for js
      (features +greet)   ; added to the global features
      (exe "hello.js.exe"))

    (target "py"
      (exe "hello.py.exe"))))
```

### Fields

| Field             | Required | Description |
| ----------------- | -------- | ----------- |
| `ribuild-version` | yes      | Must match the version of `rib` (currently `"1"`) |
| `name`            | no       | Package name |
| `description`     | no       | Package description |
| `version`         | no       | Package version |
| `authors`         | no       | List of author names |
| `entry`           | yes      | **Symbol** naming the procedure called to start the program (e.g. `main`) |
| `output-dir`      | no       | Directory where the targets are written (default: `.`) |
| `includes`        | yes      | Libraries and source files, see [Includes](#includes-and-glob-patterns) |
| `features`        | no       | Ribbit features, `+name` to enable, `-name` to disable |
| `targets`         | yes      | One `(target "<host>" ...)` form per host to compile for |

### Target fields

The target name is the Ribbit host passed to `rsc -t` (`js`, `py`, `c`, ...).

| Field                | Description |
| -------------------- | ----------- |
| `(exe "<file>")`     | Also produce an executable named `<file>` in `output-dir`. `run` uses the first target that has one |
| `(output "<file>")`  | Name of the compiled program (default: the `exe` name, or `out.<target>`) |
| `(rvm "<file>")`     | Use a custom Ribbit VM for this target (`rsc -r`) |
| `(includes ...)`     | Includes only used for this target, after the global ones (globs are supported) |
| `(features ...)`     | Features only used for this target. They override the global features with the same name (e.g. a global `+prim-no-arity` and a target `-prim-no-arity` gives `-prim-no-arity`) |

## Includes and glob patterns

Each entry of `includes` is one of:

- `(ribbit "<lib>")`: a library from the Ribbit standard library,
  e.g. `(ribbit "r4rs")` or `(ribbit "r4rs/sys")`
- `"path/to/file.scm"`: a source file, relative to the package root
- `"dir/**"`: **every `.scm` file below `dir`**, subdirectories included
- `"**"`: every `.scm` file of the package

Files matched by a glob are included in alphabetical order. The glob must be
at the end of the path (`src/**/foo.scm` is not supported).

```scheme
(includes
  (ribbit "r4rs")
  (ribbit "r4rs/sys")
  "src/**")
```

## How a build works

`rib build`:

1. Expands the `includes` (globs included) and writes the generated file
   `<entry>.scm` (e.g. `main.scm`) at the root of the package. The per-target
   includes are put in a `cond-expand` on the target host:

   ```scheme
   ;; DO NOT EDIT THIS FILE, IT IS GENERATED BY RIBUILD ON EACH BUILD

   (%%include-once (ribbit "r4rs"))
   (%%include-once "src/main.scm")

   (cond-expand
     ((host js)
      (%%include-once "js/lib.scm")
      #t)
     (else #t))

   (main)
   ```

2. Compiles it with `rsc` for each target, passing the target, its features
   and the output paths:

   ```sh
   rsc -t js -f+ prim-no-arity -f+ v-port -f+ greet -f+ ribuild -o build/hello.js.exe -x build/hello.js.exe main.scm
   ```

3. Removes `<entry>.scm`, unless `-k`/`--keep` is passed.

The `ribuild` feature is always enabled, so your code can check whether it is
being built by Ribuild with `(if-feature ribuild ...)`.

For scripts, the generated file is `__ribuild_script__.scm`, written in the
current directory, and it includes the script file at the end instead of
calling an entry procedure.

Since the per-target includes rely on the host, two targets with the same
host share their per-target includes.

**Note:** include paths are resolved relative to the file that contains the
`%%include-once`. If a source file includes another one itself, use a path
relative to that source file, not to the package root.

## Scripts

A script is a single `.scm` file that holds its own build configuration in a
commented-out (`#;`) `define-script` form, so it stays a valid Scheme file.

```sh
rib init -s hello.scm      # appends the default configuration to hello.scm
rib init -s hello.scm -c   # same, on a single line
rib run -s hello.scm       # builds the script and runs it
```

The default configuration:

```scheme
#;(define-script
  (ribuild-version "1")
  (output-dir "/tmp")
  (includes (ribbit "r4rs"))
  (features +prim-no-arity +v-port)
  (targets
    (target "js" (exe "out.js.exe"))))
```

The script file itself is used as the entry point.

## Testing

`rib test` (or `rib test -s <FILE>`) works like `rib run`, except that it:

- enables the `test` and `ribuild/test` features
- adds the `-test` suffix to the outputs, so they do not overwrite the normal
  build

Put your tests behind these features:

```scheme
(if-feature ribuild/test
  (begin
    (assert-equal (my-fn 1 2) 3 "my-fn failed")
    (display "All tests passed !\n")))
```

## Project layout

```
ribuild/
|-- package.scm        Ribuild's own package description
|-- main.scm           generated entry file, used to bootstrap `rib`
|-- makefile
|-- bin/
|   `-- rsc            wrapper around ribbit/src/rsc.exe
|-- src/
|   |-- rb.scm         command line parsing and usage (defines `main`)
|   |-- config.scm     reading and validating package.scm
|   |-- core.scm       building targets (calls rsc)
|   |-- core/deps.scm  includes and glob expansion
|   |-- utils.scm      string and list helpers
|   `-- cli/
|       |-- utils.scm  templates and file system helpers
|       `-- cmd/       one file per command (build, init, run, test, ...)
`-- templates/         templates used by `rib init`
```

## Building Ribuild itself

Ribuild is a Ribuild package. `make` bootstraps it in two steps:

1. `rsc` compiles `main.scm` (the generated entry file, which is committed)
   into a temporary `out.js`
2. `node out.js build` runs that first version of `rib` on Ribuild's own
   `package.scm`, which writes the final executable to `bin/rib`

```sh
make          # builds bin/rib
make clean    # removes bin/rib
```

## License

BSD 3-Clause, see [LICENSE](LICENSE).
