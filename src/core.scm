(define (includes-to-string includes path)
  (call-with-output-file
    path
    (lambda (port)
      (for-each (lambda (lib) (write (list '##include-once lib) port)) includes))))

(define (process-target-output target-name output quiet?)
  (if (string-prefix? "Error: " output)
    (display (string-append output "\n[ERROR] skipping target `" target-name "`\n\n"))
    (or quiet? (display (string-append (if (string=? output "") "" (string-append output "\n")) "[DONE] target `" target-name "`\n\n")))))

(define (build-target target-config config cmd-args)
  (let* ((target-name (car target-config))
         (target-exe (car (getv 'exe (cdr target-config) (list '()))))
         (target-output (car (getv 'output 
                                   (cdr target-config) 
                                   (if (null? target-exe)
                                     (list (string-append "out." target-name))
                                     (list target-exe)))))
         (entry (car (getv 'entry config)))
         (target-output-suffix (or (assocadr "target-output-suffix" cmd-args) ""))
         (target-exe-suffix (or (assocadr "target-exe-suffix" cmd-args) target-output-suffix))
         (includes (getv 'includes config) '((ribbit "empty")))
         (features (getv 'features config '()))
         (rvm (car (getv 'rvm (cdr target-config) '(())))))
    (let* ((-t (string-append "-t " target-name " "))
           (--prefix-code (begin
                            (includes-to-string includes "/tmp/__ribbit_comp__tmp_lib.scm")
                            "--prefix-code /tmp/__ribbit_comp__tmp_lib.scm "))
           (-o (string-append "-o " 
                              (or (assocadr "output" cmd-args)
                                  (string-append (car (getv 'output-dir config '("."))) "/" target-output))
                              target-output-suffix
                              " "))
           (-x (let ((x (assocadr "exe" cmd-args)))
                 (if (null? target-exe)
                   (if x (string-append "-x " x target-exe-suffix) "")
                   (string-append "-x " (or x 
                                            (string-append (car (getv 'output-dir config '("."))) "/" target-exe))
                                  target-exe-suffix
                                  " "))))
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
      ;;(pp (string-append "rsc " -t -f " -f+ ribuild " -r --prefix-code -o -x entry))
      (let ((result (shell-cmd (string-append "rsc " -t -f " -f+ ribuild " -r --prefix-code -o -x entry))))
        (process-target-output target-name result (assoc "quiet" cmd-args))))))

(define (build-library config cmd-args)
  (let* ((entry (car (getv 'entry config)))
         (includes (getv 'includes config) '((ribbit "empty")))
         (features (getv 'features config '())))
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

