(define usage 
  "`rib` - Ribuild : The Ribbit Package Manager

SYNOPSIS
`rib` <CMD> [OPTION]...

COMMANDS
PROJECT COMMANDS
`init` <PACKAGE-NAME>
Creates the <PACKAGE-NAME> directory with a package.scm, a src/main.scm,
a build and a lib directory.

`i`, `install`
Installs the `dependencies` of the project in its `dependency-dir`
(`lib` if not set). Already installed dependencies are skipped.

`b`, `build` [OPTION]...
Builds every target of the project (or only the one given with -t).

`r`, `run` [OPTION]... [-- <ARGS>...]
Builds and runs the first target that has an `exe` (or the one given
with -t).

`test` [OPTION]... [-- <ARGS>...]
Like `run`, with the `test` and `ribuild/test` features enabled. Outputs
get the `-test` suffix.

SCRIPT COMMANDS
`init` -s/--script <SCRIPT> [-c]
Appends the default script config to <SCRIPT> (on one line with -c).

`b`, `build` -s/--script <SCRIPT> [OPTION]...
Builds the script.

`r`, `run` -s/--script <SCRIPT> [OPTION]... [-- <ARGS>...]
Builds the script, then runs the first target that has an `exe`.

`test` -s/--script <SCRIPT> [OPTION]... [-- <ARGS>...]
Like `run`, with the `test` and `ribuild/test` features enabled.

OTHER
`-v`, `--version`
Prints the version of ribuild.

`-h`, `--help`
Prints this message.

OPTION
By default, targets are written in the `output-dir` of the package
(`.` if not set).

-t, --target <NAME>
With `run` and `test`, runs the target <NAME>.

-o, --output <FILE>
Writes the compiled program to <FILE> instead.

-x, --exe <FILE>
Writes the executable to <FILE> instead.

-q, --quiet
Hides the [COMPILING] and [DONE] messages.

-k, --keep
Keeps the generated entry file (<entry>.scm, or __ribuild_script__.scm for
scripts) instead of removing it after the compilation.

--target-output-suffix <SUFFIX>
Appends <SUFFIX> to the name of the compiled program.

--target-exe-suffix <SUFFIX>
Appends <SUFFIX> to the name of the executable (defaults to the output suffix).

-- <ARGS>...
With `run` and `test`, passes <ARGS> to the program.

EXAMPLES
`rib init hello`
`rib build`
`rib run -t js -- arg1 arg2`
`rib run -s script.scm`
")

(define (br-call bool-cond fn1 fn2 . args)
  (apply (if bool-cond fn1 fn2) args))

(define (parse-cmd-line args)
  (if (or (null? args) (member (car args) '("-h" "--help")))
    (begin 
      (display usage)
      (%%exit 0)))
  
  (let ((script-cmd? (and (pair? (cdr args))
                          (member (cadr args) '("-s" "--script")))))
    (cond 
      ;;((null? args) (display "*** A command must be specified. Use --help to see usage\n"))

      ((member (car args) '("b" "build"))
       (br-call script-cmd? cmd-sbuild cmd-build (cdr args)))
      ((member (car args) '("r" "run"))
       (br-call script-cmd? cmd-srun cmd-run (cdr args)))

      ((member (car args) '("i" "install"))
       (cmd-install (cdr args)))

      ((string=? (car args) "init")
       (br-call script-cmd? cmd-sinit cmd-init (cdr args)))

      ((string=? (car args) "test")
       (br-call script-cmd? cmd-stest cmd-test (cdr args)))
      ;; ((member (car args) '("sb" "sbuild"))
      ;;  (cmd-sbuild (cdr args)))
      ;; ((member (car args) '("sr" "srun"))
      ;;  (cmd-srun (cdr args)))
      ;;
      ;; ((string=? (car args) "sinit")
      ;;  (cmd-sinit (cdr args)))

      ((member (car args) '("-v" "--version"))
       (display "Ribuild v")
       (display RIBUILD-VERSION)
       (newline))

      (else (display "Invalid args")))))

(define (main)
  (if-feature ribuild/test
    (display "All tests passed !\n")
    (parse-cmd-line (cdr (cmd-line)))))
