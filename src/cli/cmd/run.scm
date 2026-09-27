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
