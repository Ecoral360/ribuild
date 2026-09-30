;; the url of the git repository of a dependency source, or #f if the source
;; isn't a git repository
(define (dependency-git-url source)
  (cond 
    ((eq? (car source) 'github) (string-append "https://github.com/" (cadr source) ".git"))
    ((eq? (car source) 'git) (cadr source))
    (else #f)))

;; a dependency has the form (name source), e.g. (srfi-180 (github "Ecoral360/ribbit-srfi-180"))
(define (install-dependency dep deps-dir)
  (let* ((name (symbol->string (car dep)))
         (source (cadr dep))
         (dest (string-append deps-dir "/" name))
         (url (dependency-git-url source)))
    (cond
      ((not url)
       (display (string-append "[ERROR] unknown source for dependency `" name "`\n")))
      ((directory? dest)
       (display (string-append "[SKIPPED] dependency `" name "` already installed in " dest "\n")))
      (else
        (display (string-append "[INSTALLING] dependency `" name "` from " url "\n"))
        ;; the `||` keeps a failed clone from making shell-cmd crash, and puts
        ;; the error message first
        (let ((result (shell-cmd (string-append "out=$(git clone --quiet " url " " dest " 2>&1)"
                                                " || { echo \"Error: cannot clone " url "\"; echo \"$out\"; }"))))
          (if (string-prefix? "Error: " result)
            (display (string-append result "[ERROR] dependency `" name "` not installed\n\n"))
            (display (string-append "[DONE] dependency `" name "` installed in " dest "\n\n"))))))))

(define (cmd-install args)
  (let* ((config   (load-pkg-config))
         (deps     (getv 'dependencies config '()))
         (deps-dir (car (getv 'dependency-dir config '("lib")))))
    (mkdir deps-dir)
    (for-each (lambda (dep) (install-dependency dep deps-dir)) deps)))
