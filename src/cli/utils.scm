(define-macro (build-dir) (cadr current-resource))


(define (get-template template-name)
  (call-with-input-file
    (string-append "../templates/" template-name ".rtscm")))
