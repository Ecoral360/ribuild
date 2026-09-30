(define (process-target-output target-name output quiet?)
  (if (string-prefix? "Error: " output)
    (display (string-append output "\n[ERROR] skipping target `" target-name "`\n\n"))
    (or quiet? (display (string-append (if (string=? output "") "" (string-append output "\n")) "[DONE] target `" target-name "`\n\n")))))

(define (feature-name feature)
  (let ((str (symbol->string feature)))
    (substring str 1 (string-length str))))

;; the target features override the global features with the same name
(define (merge-features features target-features)
  (let ((target-names (map feature-name target-features)))
    (append (filter (lambda (feature) (not (member (feature-name feature) target-names)))
                    features)
            target-features)))

(define (build-target target-config config cmd-args code)
  (let* ((target-name (car target-config))
         (target-exe (car (getv 'exe (cdr target-config) (list '()))))
         (target-output (car (getv 'output 
                                   (cdr target-config) 
                                   (if (null? target-exe)
                                     (list (string-append "out." target-name))
                                     (list target-exe)))))
         (target-output-suffix (or (assocadr "target-output-suffix" cmd-args) ""))
         (target-exe-suffix (or (assocadr "target-exe-suffix" cmd-args) target-output-suffix))
         (features (merge-features (getv 'features config '())
                                   (getv 'features (cdr target-config) '())))
         (rvm (car (getv 'rvm (cdr target-config) '(())))))
    (let* ((-t (string-append "-t " target-name " "))
           (-o (string-append "-o " 
                              (or (assocadr "output" cmd-args)
                                  (string-append (car (getv 'output-dir config '("."))) "/" target-output))
                              target-output-suffix
                              " "))
           (-x (let ((x (assocadr "exe" cmd-args)))
                 (if (null? target-exe)
                   (if x (string-append "-x " x target-exe-suffix) "")
                   (string-append "-x " (or x 
                                            (string-append (car (getv 'output-dir config '("."))) "/" target-exe))
                                  target-exe-suffix
                                  " "))))
           (-f (apply string-append (map 
                                      (lambda (feature) (string-append 
                                                          "-f" 
                                                          (string (string-ref feature 0))
                                                          " "
                                                          (substring feature 1 
                                                                     (string-length feature))
                                                          " "))
                                      (map symbol->string features))))
           (-r (if (null? rvm) "" (string-append "-r " rvm " "))))
      (or
        (assoc "quiet" cmd-args)
        (display (string-append "[COMPILING] Target `" target-name "`\n")))
      (let ((result (shell-cmd (string-append "rsc " -t -f " -f+ ribuild " -r -o -x code))))
        (process-target-output target-name result (assoc "quiet" cmd-args))))))

;; builds every target, the generated entry file is removed unless --keep is passed
(define (build-targets config cmd-args (only-target #f))
  (let* ((targets (map cdr (getv 'targets config)))
         (code (write-entry-file config targets)))
    (for-each (lambda (target-config) 
                (if only-target 
                  (and (string=? only-target (car target-config)) 
                       (build-target target-config config cmd-args code))
                  (build-target target-config config cmd-args code)))
              targets)
    (if (not (assoc "keep" cmd-args))
      (shell-cmd (string-append "rm -f " code)))))

;; (define (build-library config cmd-args)
;;   (let* ((entry (car (getv 'entry config)))
;;          (includes (getv 'includes config) '((ribbit "empty")))
;;          (features (getv 'features config '())))
;;     (let* ((-t (string-append "-t " target-name " "))
;;            (--prefix-code (begin
;;                             (includes-to-string includes)
;;                             "--prefix-code /tmp/__ribbit_comp__tmp_lib.scm "))
;;            (-o (string-append "-o " (car (getv 'output-dir config '("."))) "/" target-output " "))
;;            (-x (if (null? target-exe)
;;                  ""
;;                  (string-append "-x " (car (getv 'output-dir config '("."))) "/" target-exe " ")))
;;            (-f (apply string-append (map 
;;                                       (lambda (feature) (string-append 
;;                                                           "-f" 
;;                                                           (string (string-ref feature 0))
;;                                                           " "
;;                                                           (substring feature 1 
;;                                                                      (string-length feature))
;;                                                           " "))
;;                                       (map symbol->string features))))
;;            (-r (if (null? rvm) "" (string-append "-r " rvm " "))))
;;       ;(pp (string-append "rsc " -t --prefix-code -f -o -x entry))
;;       (or
;;         (assoc "quiet" cmd-args)
;;         (display (string-append "[COMPILING] Target `" target-name "`\n")))
;;       (let ((result (shell-cmd (string-append "rsc " -t -f " -f+ ribuild " -r --prefix-code -o -x entry))))
;;         (process-target-output target-name result (assoc "quiet" cmd-args))))))

