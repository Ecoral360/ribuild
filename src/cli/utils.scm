(define-macro (build-dir) (cadr current-resource))


(define (get-template template-name)
  (string-from-file (string-append (build-dir) "../../templates/" template-name ".rtscm")))
