(if-feature ribuild
  (begin
    (use r4rs)
    (use r4rs/sys)
    (include-once "ribuild.scm"))
  (begin
    (##include-once (ribbit "r4rs"))
    (##include-once (ribbit "r4rs/sys"))
    (##include-once "src/config.scm")
    (##include-once "src/ribuild.scm")))

(define RIBUILD-VERSIONS '("0.2.0" "0.1.0"))

(define LATEST-RIBUILD-VERSION (car RIBUILD-VERSIONS))

(define package-ribuild-version '())

(define (include-package-ribuild-version config)
  (let ((version (car (getv 'ribuild-version config))))
    (if (string=? version LATEST-RIBUILD-VERSION)
      (set! package-ribuild-version (string-append "v" version))
      (if (member version RIBUILD-VERSIONS)
        (begin 
          (display (string-append "\033[1;33m[Warning]\033[0m Using an old version of ribuild (" version "). Latest version is " LATEST-RIBUILD-VERSION))
          (set! package-ribuild-version (string-append "v" version)))
        (error "\033[0;31m[ERROR]\033[0m Unknown ribuild version (" version "). Valid versions are" (object->string RIBUILD-VERSIONS))))))

(define (process-config config)
  (if (eq? (car config) 'define-package)
    (validate-ribuild-config (cdr config))
    (error "define-package not found in package.scm")))


(define (load-pkg-config) 
  (process-config (call-with-input-file "package.scm" read)))

(define (parse-cmd-line args)
  (define usage 
"`rib` - Ribuild : The Ribbit Package Manager

SYNOPSIS
`rib` [CMD] [OPTION]...

COMMANDS
  `build`
  Builds the project

  `run`
  Builds the project and runs the first `exe` target in the project

EXAMPLE
`rib build`
")

  (cond 
    ((null? args) (display "*** A command must be specified. Use --help to see usage\n"))
    ((member (car args) '("-h" "--help"))
     (display usage))
    ((string=? (car args) "build")
     (cmd-build (cdr args)))
    ((string=? (car args) "run")
     (cmd-run (cdr args)))
    ((string=? (car args) "init")
     (cmd-init (cdr args)))
    ((member (car args) '("-v" "--version"))
     (display RIBUILD-VERSION)
     (newline))
    (else (display "Invalid args"))))


(parse-cmd-line (cdr (cmd-line)))


