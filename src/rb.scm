(if-feature (not ribuild)
  (begin
    (##include-once (ribbit "r4rs"))
    (##include-once (ribbit "r4rs/sys"))
    (##include-once "src/utils.scm")
    (##include-once "src/config.scm")
    (##include-once "src/core.scm")
    (##include-once "src/cli/utils.scm")
    (##include-once "src/cli/cmd.scm")))

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

`init` <PACKAGE-NAME> [OPTION]
Initializes the package project.

SCRIPT COMMANDS
`b`, `build` -s/--script <SCRIPT> [OPTION]
Builds the script

`init` -s/--script <SCRIPT> [OPTION]
Initializes the script.

`r`, `run` -s/--script <SCRIPT> [OPTION]
Builds the script and runs the first `exe` target in the project

OPTION

-o, --output <FILE>
By default, the target is built in a tmp directory to avoid cluttering.
Setting output will put the target there instead.

EXAMPLE
`rib build`
")

(define (br-call bool-cond fn1 fn2 . args)
  (apply (if bool-cond fn1 fn2) args))

(define (parse-cmd-line args)
  (if (or (null? args) (member (car args) '("-h" "--help")))
    (begin 
      (display usage)
      (##exit 0)))
  
  (let ((script-cmd? (and (pair? (cdr args))
                          (member (cadr args) '("-s" "--script")))))
    (cond 
      ;;((null? args) (display "*** A command must be specified. Use --help to see usage\n"))

      ((member (car args) '("b" "build"))
       (br-call script-cmd? cmd-sbuild cmd-build (cdr args)))
      ((member (car args) '("r" "run"))
       (br-call script-cmd? cmd-srun cmd-run (cdr args)))
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
       (display RIBUILD-VERSION)
       (newline))

      (else (display "Invalid args")))))


(if-feature ribuild/test
  (display "All tests passed !\n")
  (parse-cmd-line (cdr (cmd-line))))




