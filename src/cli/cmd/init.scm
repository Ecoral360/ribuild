(define (cmd-init args)
  (let* ((package-name (if (null? args) 
                         (error "*** You must specify a name to your package") 
                         (car args)))
         (template (get-template "init"))
         (processed-template (string-replace*
                               template
                               (list "${ribuild-version}" RIBUILD-VERSION)
                               (list "${pkg-name}" package-name)
                               (list "${author}" "John Doe"))))
    (mkdir package-name)
    (if (file-exists? (string-append package-name "/package.scm"))
      (error "Cannot create package.scm because a package.scm is already defined for this package.")
      (begin
        (call-with-output-file
          (string-append package-name "/package.scm")
          (lambda (output-port)
            (display processed-template output-port)))
        (mkdir (string-append package-name "/src") (string-append package-name "/build"))
        (call-with-output-file
          (string-append package-name "/src/main.scm")
          (lambda (output-port)
            (write '(define (main) (display "Hello from Ribuild!\n")) output-port)))))))

(define (cmd-sinit args)
  (let* ((script-file (if (null? (cdr args))
                        (error "*** You must specify a script file") 
                        (cadr args)))
         (concise? (member "-c" args))
         (template (get-template "script"))
         (content (string-from-file script-file))
         (config-idx (string-find content "#;(define-script"))) ;;)

    (if config-idx
      (error "Ribbit script config already found in the file."))

    (call-with-output-file
      script-file
      (lambda (output-port)
        (display content output-port)
        (newline output-port)
        (display "#;" output-port)
        (display (if concise?
                   (string-concatenate (string-split template #\newline) "")
                   template)
                 output-port)))))

