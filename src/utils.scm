(define-macro 
  (let+ vars-value* . body)

  (define (range n) 
    (if (= n 0)
      (list n) 
      (append (range (- n 1)) (list n))))

  (define (symbol-prefix? prefix sym)
    (let ((s-sym (symbol->string sym))
          (p-len (string-length prefix)))
      (and (>= (string-length s-sym) p-len)
           (string=? (substring s-sym 0 p-len) prefix))))

  (let ((value-sym* (map (lambda (_) (gensym)) vars-value*)))
    `(let* (,@(apply append 
                (map 
                  (lambda (value-sym vars-value) 
                    (cond 
                      ((symbol? (car vars-value))
                       `((,(car vars-value) ,(cadr vars-value))))

                      ((and (pair? (car vars-value)) (memq #t (map symbol? vars-value))) 
                       `((,value-sym ,(cadr vars-value))
                         (,(gensym)
                           (if (>= (length ,value-sym) ,(length (car vars-value))) 
                             #t
                             (error ,(string-append "Not enough values to unpack " 
                                                    (object->string vars-value)))))
                         ,@(map 
                             (lambda (var i)
                               `(,var (,(if (symbol-prefix? "*" var)
                                          'list-tail
                                          'list-ref)
                                        ,value-sym 
                                        ,i)))
                             (car vars-value)
                             (range (- (length (car vars-value)) 1)))))
                      (else
                        (error "Must be an intendifier or a list of identifiers."))))
                  value-sym* vars-value*)))
       ,@body)))

(define (assocadr val lst)
  (let ((x (assoc val lst)))
    (and x (cadr x))))

(define (take-while predicate lst)
  (let loop ((final '())
             (rest lst))
    (cond 
      ((null? rest) final)
      ((not (predicate (car rest))) final)
      (else (loop (append final (list (car rest))) (cdr rest))))))

(define (assert test msg)
  (if (not test)
    (error msg)))

(define (assert-equal actual expected msg)
  (if (not (equal? actual expected))
    (error msg "Expected:" expected ", Got:" actual)))

(define (display->file filename content)
  (call-with-output-file
    filename
    (lambda (output-port)
      (display content output-port))))

(define (write->file filename content)
  (call-with-output-file
    filename
    (lambda (output-port)
      (write content output-port))))

(define (map-find fn lst)
  (if (pair? lst)
    (let ((result (fn (car lst))))
      (if result 
        (list result (car lst))
        (find fn (cdr lst)))
      #f)))

(define (##list->string-opt lst len) (##rib lst len string-type))
(define (substring-tail-view str start)
  (##list->string-opt (list-tail (##field0 str) start) (##- (##field1 str) start)))

(define (string-replace str substr repl)
  (define substr-len (string-length substr))
  (define (inner-string-replace str final)
    (if (= (##field1 str) 0)
      final
      (let ((match (string-prefix? substr str)))
        (if match
          (inner-string-replace 
            (substring-tail-view str substr-len)
            (string-append final repl))
          (inner-string-replace
            (substring-tail-view str 1)
            (##list->string-opt 
             (append (##field0 final) (##field0 (##field0 str))) 
             (##+ (##field1 final) 1)))))))

  (inner-string-replace str ""))

(define (string-replace* str . substr-repl*)
  (define (inner-string-replace str final)
    (if (string=? str "")
      final
      (let ((pat (find (lambda (len-substr-repl) (string-prefix? (cadr len-substr-repl) str))
                       substr-repl*)))
        (if pat
          (inner-string-replace 
            (substring-tail-view str (car pat))
            (string-append final (caddr pat)))
          (inner-string-replace
            (substring-tail-view str 1)
            (string-append final (string (car (string->list str)))))))))

  (set! substr-repl* (map (lambda (substr-repl) (cons (string-length (car substr-repl)) substr-repl))
                          substr-repl*))

  (inner-string-replace str ""))

(if-feature ribuild/test
  (begin
    (assert-equal 
      (string-replace* "ab ba ca ba ooo" (list "ba" "x") (list "ca" "op"))
      "ab x op x ooo"
      "Failed `string-replace*`")))
