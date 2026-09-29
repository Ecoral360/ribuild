(define (cmd-build args)
  (build-targets (load-pkg-config) (cmd-run-process-args args)))

(define (cmd-sbuild args)
  (let* ((script-file (cadr args))
         (config (load-script-config script-file)))
    (build-targets config (cmd-run-process-args (cddr args)))))
