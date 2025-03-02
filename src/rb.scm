(if-feature (not ribuild)
  (begin
    (##include-once (ribbit "r4rs"))
    (##include-once (ribbit "r4rs/sys"))
    (##include-once "src/utils.scm")
    (##include-once "src/config.scm")
    (##include-once "src/core.scm")
    (##include-once "src/cli/cmd.scm")))


(define (parse-cmd-line args)
  (define usage 
"`rib` - Ribuild : The Ribbit Package Manager

SYNOPSIS
`rib` <CMD> [OPTION]...

COMMANDS
  PROJECT COMMANDS
  `b`, `build`
  Builds the project

  `r`, `run`
  Builds the project and runs the first `exe` target in the project

  SCRIPT COMMANDS
  `sb`, `sbuild` <SCRIPT> [OPTION]
  Builds the script

  `sr`, `srun` <SCRIPT> [OPTIONS]
  Builds the script and runs the first `exe` target in the project

    OPTIONS

    -o, --output <FILE>
    By default, the target is built in a tmp directory to avoid cluttering.
    Setting output will put the target there instead.

EXAMPLE
`rib build`
")

  (cond 
    ;;((null? args) (display "*** A command must be specified. Use --help to see usage\n"))
    ((or (null? args) (member (car args) '("-h" "--help")))
     (display usage))

    ((member (car args) '("b" "build"))
     (cmd-build (cdr args)))
    ((member (car args) '("r" "run"))
     (cmd-run (cdr args)))
    ((string=? (car args) "init")
     (cmd-init (cdr args)))

    ((member (car args) '("sb" "sbuild"))
     (cmd-sbuild (cdr args)))
    ((member (car args) '("sr" "srun"))
     (cmd-srun (cdr args)))
    ((string=? (car args) "sinit")
     (cmd-sinit (cdr args)))

    ((member (car args) '("-v" "--version"))
     (display RIBUILD-VERSION)
     (newline))
    (else (display "Invalid args"))))


(parse-cmd-line (cdr (cmd-line)))


