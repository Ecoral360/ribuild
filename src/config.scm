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

