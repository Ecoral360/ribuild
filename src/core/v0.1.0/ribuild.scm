(define (run-rsc entry . options)
  (let ((ribbit-version-feature (string-append "-f+ ribuild/v" RIBUILD-VERSION " ")))
    (shell-cmd (string-append "rsc " (apply string-append options) " -f+ ribuild " ribbit-version-feature entry))))

(define (includes-to-string libraries)
  (call-with-output-file
    "/tmp/__ribbit_comp__tmp_lib.scm"
    (lambda (port)
      (for-each (lambda (lib) (write (list '##include-once lib) port)) libraries))))

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
         (libraries (getv 'libraries config '((ribbit "empty"))))
         (features (getv 'features config '()))
         (rvm (car (getv 'rvm (cdr target-config) '(())))))
    (let* ((-t (string-append "-t " target-name " "))
           (--prefix-code (begin
                            (includes-to-string libraries)
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
      (let ((result (run-rsc entry -t -f -r --prefix-code -o -x)))
        (process-target-output target-name result (assoc "quiet" cmd-args))))))

(define (cmd-build args)
  (let* ((config (load-pkg-config))
         (targets (getv 'targets config))
         (cmd-args '()))
    (for-each 
      (lambda (target-config) (build-target target-config config cmd-args)) 
    (map cdr targets))))

(define (cmd-run-process-args args)
  (let loop ((cmd-args '())
             (rest args))
    (if (null? rest)
      cmd-args
      (let ((arg (car rest)))
        (cond
          ((member arg '("-q" "--quiet")) 
           (loop (cons '("quiet" #t) cmd-args) (cdr rest)))
          ((member arg '("-t" "--target"))
           (loop (cons '("target" (cadr rest)) cmd-args) (cddr rest)))
          (else 
            (display (string-append "Ignoring unknown option '" arg "'"))))))))

(define (take-while predicate lst)
  (let loop ((final '())
             (rest lst))
    (cond 
      ((null? rest) final)
      ((not (predicate (car rest))) final)
      (else (loop (append final (list (car rest))) (cdr rest))))))

(define (cmd-run args)
  (let* ((config (load-pkg-config))
         (targets (getv 'targets config))
         (cmd-args (cmd-run-process-args (take-while (lambda (arg) (not (string=? "--" arg))) args)))
         (target-name (let ((t (assoc "target" cmd-args)))
                        (and t (cadr t))))
         (target-exe (find (lambda (target) 
                             (and
                               (or
                                 (not target-name)
                                 (string=? (begin (write target) (car target)) target-name))
                             (getv 'exe (cdr target) #f)))
                           (map cdr targets)))
         (_ (if (not target-exe) (error "Error: cannot run, exe target not found") '()))
         (target-exe-path 
           (string-append (car (getv 'output-dir config '("."))) "/" (car (getv 'exe (cdr target-exe))))))
    (for-each 
      (lambda (target-config) (build-target target-config config cmd-args)) 
      (map cdr targets))
    (let* ((exe-args (if (null? args) '() (member "--" args)))
           (exe-args-str (if (pair? exe-args) (string-concatenate (cdr exe-args) " ") "")))
      (display (shell-cmd target-exe-path exe-args-str)))))

(define-macro (build-dir) (cadr current-resource))

(define init-template (##include-string "../templates/init.scm"))

(define (cmd-init args)
  (if (file-exists? "package.scm")
    (error "Cannot create package.scm because a package.scm is already defined in this directory.")
    (let* ((package-name (if (null? args) 
                           (error "*** You must specify a name to your package") 
                           (car args)))
           (template (string-concatenate 
                       (string-split init-template #\$)
                       package-name)))
      (call-with-output-file
        "package.scm"
        (lambda (output-port)
          (display template output-port)))
      (call-with-output-file
        "main.scm"
        (lambda (output-port)
          (write '(display "Hello from Ribuild!\n") output-port))))))
