(define (cmd-init args)
  (if (file-exists? "package.scm")
    (error "Cannot create package.scm because a package.scm is already defined in this directory.")
    (let* ((package-name (if (null? args) 
                           (error "*** You must specify a name to your package") 
                           (car args)))
           (template (get-template "init"))
           (processed-template (string-replace*
                                 template
                                 (list "${pkg-name}" package-name)
                                 (list "${author}" "John Doe"))))
      (call-with-output-file
        "package.scm"
        (lambda (output-port)
          (display processed-template output-port)))
      (shell-cmd "mkdir -p src build")
      (call-with-output-file
        "src/main.scm"
        (lambda (output-port)
          (write '(display "Hello from Ribuild!\n") output-port))))))

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

