(define-package 
  (ribuild-version "0.2.0")

  (name "ribuild")
  (description "A tool to build ribbit projects.")
  (version "0.1.0")
  (authors ("Mathis Laroche"))

  (entry "src/rb.scm")
  (output-dir "bin") ; specify the dir where to put the output of targets

  ;; libraries have the form accepted by the ##include-once ribbit directive
  (libraries
    (r4rs "0.3.4" (ribbit "r4rs"))
    (r4rs/sys "0.3.4" (ribbit "r4rs/sys"))
    "src/config.scm"
    "src/ribuild.scm")

  (features 
    +v-port
    +prim-no-arity  ; prefix `+` sets the feature value to #t
    -js/web)        ; prefix `-` sets the feature value to #f

  (targets
    (target "js" ; adds javascript as a target of the package
      (exe "rib")))) ;; overrides the default name given to output program
