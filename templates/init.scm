(define-package  
  ; don't change the ribuild-version yourself, automatically set by ribuild
  (ribuild-version "0.1.0")

  (name "$")
  (description "Add your description here !")
  (version "0.1.0")
  (authors ("John Doe"))

  (entry "main.scm") ; specify the entry point of the program
  (output-dir ".") ; specify the dir where to put the output of targets

  ;; libraries have the form accepted by the ##include-once ribbit directive:
  ;; (ribbit "std-lib") OR "path/to/file.scm"
  (libraries
    (ribbit "r4rs"))

  (features ; sets the features for all the targets
    +prim-no-arity  ; prefix `+` sets the feature value to #t
    -other-feature) ; prefix `-` sets the feature value to #f

  (targets
    (target "py" ; adds python as a target of the package
      (out "$-py.scm") ; overrides the default name given to output program
      (exe "$.py.exe"))   ; adds python as a target of the package

    (target "js" ; adds javascript as a target of the package
      (out "$-js.scm") ; overrides the default name given to output program
      (exe "$.js.exe")))) ; creates an executable for the target (not available for all target)
