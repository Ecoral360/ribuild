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
