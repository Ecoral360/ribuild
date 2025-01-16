(define RIBUILD-VERSION "0.1.0")

(define (validate-ribuild-version config)
  (let ((version (car (getv 'ribuild-version config))))
    (if (string=? version RIBUILD-VERSION)
      config
      (error "Wrong version (expected" RIBUILD-VERSION "but found" version ")"))))

(define (process-config config)
  (if (eq? (car config) 'define-package)
    (validate-ribuild-version (cdr config))
    (error "package.scm must only contain a call to define-package")))


(define (load-pkg-config) 
  (process-config (call-with-input-file "package.scm" read)))


(define noparams (##rib 0 0 5))

(define (getv key config (default noparams))
  (let ((pair (assq key config)))
    (if (pair? pair)
      (cdr pair)
      (if (eq? default noparams)
        (error "Missing required field" key "in package.scm")
        default))))

(define (includes-to-string libraries)
  (call-with-output-file
    "/tmp/__ribbit_comp__tmp_lib.scm"
    (lambda (port)
      (for-each (lambda (lib) (write (list '##include-once lib) port)) libraries))))

(define (process-target-output target-name output)
  (if (string-prefix? "Error: " output)
    (display (string-append output "\n[ERROR] skipping target `" target-name "`\n\n"))
    (display (string-append (if (string=? output "") "" (string-append output "\n")) "[DONE] target `" target-name "`\n\n"))))

(define (build-target target-config config)
  (let* ((target-name (car target-config))
         (target-exe (car (getv 'exe (cdr target-config) '(()))))
         (target-output (car (getv 'output 
                                   (cdr target-config) 
                                   (if (null? target-exe)
                                     (list (string-append "out." target-name))
                                     (list target-exe)))))
         (entry (car (getv 'entry config)))
         (libraries (getv 'libraries config) '((ribbit "empty")))
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
      (display (string-append "[COMPILING] Target `" target-name "`\n"))
      (let ((result (shell-cmd (string-append "rsc " -t -f -r --prefix-code -o -x entry))))
        (process-target-output target-name result)))))

(define (cmd-build args)
  (let* ((config (load-pkg-config))
         (targets (getv 'targets config)))
    (for-each 
      (lambda (target-config) (build-target target-config config)) 
    (map cdr targets))))

(define (cmd-run args)
  (let* ((config (load-pkg-config))
         (targets (getv 'targets config))
         (target-exe (find (lambda (target) (getv 'exe (cdr target) #f)) (map cdr targets)))
         (_ (if (not target-exe) (error "No exe target to run") '()))
         (target-exe-path 
           (string-append (car (getv 'output-dir config '("."))) "/" (car (getv 'exe (cdr target-exe))))))
    (for-each 
      (lambda (target-config) (build-target target-config config)) 
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
          (display template output-port))))))
