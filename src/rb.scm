(##include-once (ribbit "r4rs"))
(##include-once (ribbit "r4rs/sys"))
(##include-once "src/ribuild.scm")

(define (parse-cmd-line args)
  (define usage 
"`rib` - Ribuild : The Ribbit Package Manager

SYNOPSIS
`rib` [CMD] [OPTION]...

COMMANDS
  `build`
  Builds the project

EXAMPLE
`rib build`
")

  (cond 
    ((null? args) (display "*** A command must be specified. Use --help to see usage\n"))
    ((member (car args) '("-h" "--help"))
     (display usage))
    ((string=? (car args) "build")
     (cmd-build (cdr args)))
    ((string=? (car args) "init")
     (cmd-init (cdr args)))
    (else (display "Invalid args"))))



(parse-cmd-line (cdr (cmd-line)))


