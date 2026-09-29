(define-package 
  (ribuild-version "0.1.0")

  (name "ribuild")
  (description "A tool to build ribbit projects.")
  (version "0.1.0")
  (authors ("Mathis Laroche"))

  (entry main)
  (output-dir "bin") ; specify the dir where to put the output of targets

  ;; libraries have the form accepted by the %%include-once ribbit directive
  (includes
    (ribbit "r4rs")
    (ribbit "r4rs/sys")
    "src/**")

  (features 
    +prim-no-arity
    +v-port
    -js/web)        ; prefix `-` sets the feature value to #f

  (targets
    (target "js" ; adds javascript as a target of the package
      (exe "rib")))) ;; overrides the default name given to output program
