(define (cmd-build args)
  (let* ((config (load-pkg-config))
         (targets (getv 'targets config))
         (cmd-args args))
    (for-each 
      (lambda (target-config) (build-target target-config config cmd-args)) 
    (map cdr targets))))

(define (cmd-sbuild args)
  (let* ((script-file (cadr args))
         (config (load-script-config script-file))
         (targets (getv 'targets config))
         (cmd-args (cddr args)))
    (for-each 
      (lambda (target-config) (build-target target-config config cmd-args)) 
    (map cdr targets))))

(define (cmd-run args)
  (_cmd-run args (load-pkg-config)))

(define (cmd-srun args)
  (assert (pair? args) "*** Script name missing.")

  (let* ((script-file (cadr args))
         (args (cddr args))
         (config (load-script-config script-file)))
    (_cmd-run args config)))

(define (_cmd-run args config)
  (let* ((targets (getv 'targets config))
         (cmd-args (cmd-run-process-args (take-while (lambda (arg) (not (string=? "--" arg))) args)))
         (target-name (let ((t (assoc "target" cmd-args)))
                        (and t (cadr t))))
         (target-output-suffix (or (assocadr "target-output-suffix" cmd-args) ""))
         (target-exe-suffix (or (assocadr "target-exe-suffix" cmd-args) target-output-suffix))
         (target-exe (find (lambda (target) 
                             (and
                               (or
                                 (not target-name)
                                 (string=? (begin (write target) (car target)) target-name))
                               (getv 'exe (cdr target) #f)))
                           (map cdr targets)))
         (_ (if (not target-exe) (error "Error: cannot run, exe target not found") '()))
         (target-exe-path 
           (string-append (car (getv 'output-dir config '("."))) "/" (car (getv 'exe (cdr target-exe))) target-exe-suffix)))
    (for-each 
      (lambda (target-config) (build-target target-config config cmd-args)) 
      (map cdr targets))
    (let* ((exe-args (if (null? args) '() (member "--" args)))
           (exe-args-str (if (pair? exe-args) (string-concatenate (cdr exe-args) " ") "")))
      (display (shell-cmd target-exe-path exe-args-str)))))

(define (cmd-run-process-args args)
  (let loop ((cmd-args '())
             (rest args))
    (if (null? rest)
      cmd-args
      (let ((arg (car rest)))
        (cond
          ((member arg (list "-q" "--quiet")) 
           (loop (cons (list "quiet" #t) cmd-args) (cdr rest)))
          ((member arg (list "-t" "--target"))
           (loop (cons (list "target" (cadr rest)) cmd-args) (cddr rest)))
          ((member arg (list "-x" "--exe"))
           (loop (cons (list "exe" (cadr rest)) cmd-args) (cddr rest)))
          ((member arg (list "-o" "--output"))
           (loop (cons (list "output" (cadr rest)) cmd-args) (cddr rest)))
          ((member arg (list "--target-output-suffix"))
           (loop (cons (list "target-output-suffix" (cadr rest)) cmd-args) (cddr rest)))
          ((member arg (list "--target-exe-suffix"))
           (loop (cons (list "target-exe-suffix" (cadr rest)) cmd-args) (cddr rest)))
          (else 
            (display (string-append "Ignoring unknown option '" arg "'"))))))))

;(define init-template (##include-string "../../templates/init.scm"))

(define (cmd-init args)
  (if (file-exists? "package.scm")
    (error "Cannot create package.scm because a package.scm is already defined in this directory.")
    (let* ((package-name (if (null? args) 
                           (error "*** You must specify a name to your package") 
                           (car args)))
           (template (get-template "init"))
           (processed-template (string-replace*
                                 template
                                 (list "${pkg-name}" package-name)
                                 (list "${author}" "John Doe"))))
      (call-with-output-file
        "package.scm"
        (lambda (output-port)
          (display template output-port)))
      (call-with-output-file
        "main.scm"
        (lambda (output-port)
          (write '(display "Hello from Ribuild!\n") output-port))))))

(define (cmd-sinit args)
  (let* ((script-file (if (null? (cdr args))
                        (error "*** You must specify a script file") 
                        (cadr args)))
         (concise? (member "-c" args))
         (template (get-template "script"))
         (content (string-from-file script-file))
         (config-idx (string-find content "#;(define-script"))) ;;)

    (if config-idx
      (error "Ribbit script config already found in the file."))

    (call-with-output-file
      script-file
      (lambda (output-port)
        (display content output-port)
        (newline output-port)
        (display "#;" output-port)
        (display (if concise?
                   (string-concatenate (string-split template #\newline) "")
                   template)
                 output-port)))))

(define (add-feature-flag config . flags)
  (let ((features (assq 'features (cdr config))))
    (if features
      (set-cdr! features (append flags (cdr features)))
      (set-cdr! config (cons `(features ,@flags) (cdr config))))
    config))

(define (cmd-test args)
  (_cmd-run (append (list "--target-output-suffix" "-test") args) (add-feature-flag (load-pkg-config) '+test '+ribuild/test)))

(define (cmd-stest args)
  (assert (pair? args) "*** Script name missing.")

  (let* ((script-file (cadr args))
         (args (cddr args))
         (config (add-feature-flag (load-script-config script-file) '+test '+ribuild/test)))
    (_cmd-run (append (list "--target-output-suffix" "-test") args) config)))
