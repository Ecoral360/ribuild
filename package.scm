(define-package 
  (ribuild-version "0.1.0")

  (name "ribuild")
  (description "A tool to build ribbit projects.")
  (version "0.1.0")
  (authors ("Mathis Laroche"))

  (entry "src/rb.scm")
  (output-dir "out") ; specify the dir where to put the output of targets

  ;; libraries have the form accepted by the ##include-once ribbit directive
  (libraries
    (ribbit "r4rs")
    (ribbit "r4rs/sys")
    "src/ribuild.scm")

  (features 
    +v-port
    +prim-no-arity  ; prefix `+` sets the feature value to #t
    -js/web)        ; prefix `-` sets the feature value to #f

  (targets
    (target "py" ; adds python as a target of the package
      (features
        -prim-no-arity) ; overrides the global feature
      (rvm "rvms/py/rvm.py")) ; specify a custom rvm for that target

    (target "js" ; adds javascript as a target of the package
      (exe "rb") ;; overrides the default name given to output program
      (libraries ; libraries used only with this target
        "./lib/a-js-lib.scm"))))

;; futur plan
;; dependencies have the form
;; (package-name (reader-type "resource-value") [version])
;; OR
;; (package-name "a/relative/path" [version])
#;(dependencies
  (r4rs (ribbit "r4rs"))
  (http-resource (ribbit "resource/http")))

