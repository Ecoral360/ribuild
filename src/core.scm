(define (includes-to-string includes)
  (call-with-output-file
    "/tmp/__ribbit_comp__tmp_lib.scm"
    (lambda (port)
      (for-each (lambda (lib) (write (list '##include-once lib) port)) includes))))

(define (process-target-output target-name output quiet?)
  (if (string-prefix? "Error: " output)
    (display (string-append output "\n[ERROR] skipping target `" target-name "`\n\n"))
    (or quiet? (display (string-append (if (string=? output "") "" (string-append output "\n")) "[DONE] target `" target-name "`\n\n")))))

(define (build-target target-config config cmd-args)
  (let* ((target-name (car target-config))
         (target-exe (car (getv 'exe (cdr target-config) '(()))))
         (target-output (car (getv 'output 
                                   (cdr target-config) 
                                   (if (null? target-exe)
                                     (list (string-append "out." target-name))
                                     (list target-exe)))))
         (entry (car (getv 'entry config)))
         (includes (getv 'includes config) '((ribbit "empty")))
         (features (getv 'features config '()))
         (rvm (car (getv 'rvm (cdr target-config) '(())))))
    (let* ((-t (string-append "-t " target-name " "))
           (--prefix-code (begin
                            (includes-to-string includes)
                            "--prefix-code /tmp/__ribbit_comp__tmp_lib.scm "))
           (-o (string-append "-o " (car (getv 'output-dir config '("."))) "/" target-output " "))
           (-x (if (null? target-exe)
                 ""
                 (string-append "-x " (car (getv 'output-dir config '("."))) "/" target-exe " ")))
           (-f (apply string-append (map 
                                      (lambda (feature) (string-append 
                                                          "-f" 
                                                          (string (string-ref feature 0))
                                                          " "
                                                          (substring feature 1 
                                                                     (string-length feature))
                                                          " "))
                                      (map symbol->string features))))
           (-r (if (null? rvm) "" (string-append "-r " rvm " "))))
      ;(pp (string-append "rsc " -t --prefix-code -f -o -x entry))
      (or
        (assoc "quiet" cmd-args)
        (display (string-append "[COMPILING] Target `" target-name "`\n")))
      (let ((result (shell-cmd (string-append "rsc " -t -f " -f+ ribuild " -r --prefix-code -o -x entry))))
        (process-target-output target-name result (assoc "quiet" cmd-args))))))
