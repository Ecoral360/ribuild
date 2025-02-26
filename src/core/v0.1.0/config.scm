(define OLD-RIBUILD-VERSION '("0.1.0"))

(define RIBUILD-VERSION "0.2.0")

(define (validate-ribuild-config config)
  (or 
    (not (getv 'includes config #f))
    (error "The `includes` field in package.scm is the replacement to the `libraries` field. \
Change the `ribuild-version` value to 0.2.0 to use the `includes` field."))
  (let ((version (car (getv 'ribuild-version config))))
    (if (string=? version RIBUILD-VERSION)
      config
      (if (member version OLD-RIBUILD-VERSION)
        (display (string-append "\033[1;33mRibuild has a new version, change to " RIBUILD-VERSION "\033[0m"))
        (error "Wrong version (expected" RIBUILD-VERSION "but found" version ")")))))

(define (process-config config)
  (if (eq? (car config) 'define-package)
    (validate-ribuild-config (cdr config))
    (error "package.scm must only contain a call to define-package")))


(define (load-pkg-config) 
  (process-config (call-with-input-file "package.scm" read)))

(define noparams (##rib 0 0 5))

(define (getv key config (default noparams))
  (let ((pair (assq key config)))
    (if (pair? pair)
      (cdr pair)
      (if (eq? default noparams)
        (error "Missing required field" (string-append "`" (object->string key) "`") "in package.scm")
        default))))

