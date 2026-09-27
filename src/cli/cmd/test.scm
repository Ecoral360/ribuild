(define (add-feature-flag config . flags)
  (let ((features (assq 'features (cdr config))))
    (if features
      (set-cdr! features (append flags (cdr features)))
      (set-cdr! config (cons `(features ,@flags) (cdr config))))
    config))

(define (cmd-test args)
  (_cmd-run (append (list "--target-output-suffix" "-test") args)
            (add-feature-flag 
              (load-pkg-config)
              '+test 
              '+ribuild/test)))

(define (cmd-stest args)
  (assert (pair? args) "*** Script name missing.")

  (let* ((script-file (cadr args))
         (args (cddr args))
         (config (add-feature-flag 
                   (load-script-config script-file)
                   '+test 
                   '+ribuild/test)))
    (_cmd-run (append (list "--target-output-suffix" "-test") args) config)))
