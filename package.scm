(define-package 
  (ribuild-version "0.1.0")

  (name "ribuild")
  (description "A tool to build ribbit projects.")
  (version "0.1.0")
  (authors ("Mathis Laroche"))

  (entry "src/rb.scm")
  (output-dir "bin") ; specify the dir where to put the output of targets

  ;; libraries have the form accepted by the ##include-once ribbit directive
  (includes
    (ribbit "r4rs")
    (ribbit "r4rs/sys")
    "src/utils.scm"
    "src/config.scm"
    "src/core.scm"
    "src/cli/cmd.scm")

  (features 
    +v-port
    -js/web)        ; prefix `-` sets the feature value to #f

  (targets
    (target "js" ; adds javascript as a target of the package
      (exe "rib")))) ;; overrides the default name given to output program
