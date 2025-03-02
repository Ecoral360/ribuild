(define-package  
  ; don't change the ribuild-version yourself, automatically set by ribuild
  (ribuild-version "0.1.0")

  (name "$NAME$")
  (description "$DESCRIPTION$")
  (version "0.1.0")
  (authors ("$AUTHOR$"))

  (entry "main.scm") ; specify the entry point of the program
  (output-dir ".") ; specify the dir where to put the output of targets

  ;; libraries have the form accepted by the ##include-once ribbit directive:
  ;; (ribbit "std-lib") OR "path/to/file.scm"
  (libraries
    (ribbit "r4rs"))

  (features 
    +prim-no-arity  ; prefix `+` sets the feature value to #t
    -other-feature) ; prefix `-` sets the feature value to #f

  (targets
    (target "py" ; adds python as a target of the package
      (features
        -prim-no-arity) ; overrides the global feature
      (libraries ; libraries used only with this target
        "mylib.scm")
      (rvm "rvms/py/rvm.py")) ; specify a custom rvm for that target

    (target "js" ; adds javascript as a target of the package
      (out "$NAME$-js.scm") ; overrides the default name given to output program
      (exe "$NAME$.exe")))) ; creates an executable for the target (not available for all target)
