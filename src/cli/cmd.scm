(define (cmd-build args)
  (let* ((config (load-pkg-config))
         (targets (getv 'targets config))
         (cmd-args args))
    (for-each 
      (lambda (target-config) (build-target target-config config cmd-args)) 
    (map cdr targets))))

(define (cmd-test args)
  (cmd-run `("-f+" "test" ,@args)))


(define (cmd-run args)
  (_cmd-run args (load-pkg-config)))

(define (cmd-srun args)
  (assert (pair? args) "*** Script name missing.")

  (let ((script-file (car args))
        (args (cdr args)))
    (_cmd-run args (load-script-config script-file))))

(define (_cmd-run args config)
  (let* ((targets (getv 'targets config))
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
