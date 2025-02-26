#lang racket

;; Author: Evan Bourgoine

;; main function to handle input and the state
(define (read-input-and-process)
  (define initial-state (list '() '() #f))
  (process-input initial-state)) ; process input

;; recursive function to process the input.
(define (process-input state)
  (let ((line (read-line)))
    (if (eof-object? line)
        (display-final-state state) ; display when input ends.
        (process-input (process-line line state))))) ; process line

;; process a single line of input, update after
(define (process-line line state)
  (match state
    [(list samples symbol-table post-fn)
     (cond
       ;; list the temperature readings.
       [(string-ci=? line "dl")
        (display-data samples)
        state]

       ;; clear all the temperature readings
       [(string-ci=? line "dc")
        (list '() symbol-table post-fn)]

       ;; list all the functions in the table
       [(string-ci=? line "pl")
        (display-symbol-table symbol-table)
        state]

       ;; clear all functions
       [(string-ci=? line "pc")
        (list samples '() post-fn)]

       ;; delete function by name
       [(and (>= (string-length line) 3) (string-ci=? (substring line 0 2) "pd"))
        (let ((identifier (string-trim (substring line 2))))
          (list samples (remove-from-symbol-table symbol-table identifier) post-fn))]

       ;; create a new post process function
       [(and (>= (string-length line) 2) (char-ci=? (string-ref line 0) #\p))
        (let ((fn-body (string-trim (substring line 1))))
          (let ((new-fn (eval (read (open-input-string (string-append "(lambda (x) " fn-body ")")))
                              (make-base-namespace))))
            (if (procedure? new-fn)
                (list samples
                      (add-to-symbol-table symbol-table "last_fn" fn-body)
                      new-fn)
                state)))]

       ;; save the function with a name.
       [(and (>= (string-length line) 2) (char-ci=? (string-ref line 0) #\s))
        (let ((identifier (string-trim (substring line 1))))
          (if post-fn
              (list samples
                    (add-to-symbol-table symbol-table identifier (cdr (assoc "last_fn" symbol-table)))
                    post-fn)
              state))]

       ;; this will reuse a previously saved post-processing function
       [(and (>= (string-length line) 2) (char-ci=? (string-ref line 0) #\r))
        (let ((fn-body (cdr (assoc (string-trim (substring line 1)) symbol-table))))
          (if fn-body
              (list samples
                    symbol-table
                    (eval (read (open-input-string (string-append "(lambda (x) " fn-body ")")))
                          (make-base-namespace)))
              state))]

       ;; i will clear the post-processing function
       [(string-ci=? line "i")
        (list samples symbol-table #f)]

       ;; when e is pressed, end the program
       [(string-ci=? line "E")
        (display-data samples)
        (exit)]

       ;; process a temperature
       [else
        (let ((result (extract-scale-and-value line)))
          (if result
              (let* ((scale (if (car result) (car result) #\F)) ; Default to Fahrenheit
                     (value (cadr result))
                     (f-temp (convert-to-fahrenheit scale value))
                     (processed-temp (if (and post-fn (number? f-temp))
                                         (post-fn f-temp)
                                         f-temp)))
                (if (valid-temperature? processed-temp)
                    (list (cons processed-temp samples) symbol-table post-fn)
                    state))
              state))])]))

;; function that will convert the temperature
(define (convert-to-fahrenheit scale value)
  (cond
    [(and (char? scale) (or (char=? scale #\F) (char=? scale #\f))) value]
    [(and (char? scale) (or (char=? scale #\C) (char=? scale #\c))) (+ (* (/ 9 5) value) 32)]
    [(and (char? scale) (or (char=? scale #\K) (char=? scale #\k))) (+ (* (/ 9 5) (- value 273.15)) 32)]
    [else #f]))

;;check if temperature is valid.
(define (valid-temperature? temp)
  (and temp (number? temp) (>= temp -459.67)))

;;get the value from the input.
(define (extract-scale-and-value line)
  (let* ((scale (if (and (>= (string-length line) 1)
                         (string-contains? "fFcCkK" (substring line 0 1)))
                    (string-ref line 0)
                    #f))
         (value-str (if scale (substring line 1) line))
         (value (string->number (string-trim value-str))))
    (if value (list scale value) #f)))

;; add a kv-pair to the table
(define (add-to-symbol-table table key value)
  (cons (cons key value) (remove-from-symbol-table table key)))

;; remove the pair from the table, searched by the key.
(define (remove-from-symbol-table table key)
  (filter (lambda (pair) (not (equal? (car pair) key))) table))

;; display the functions
(define (display-symbol-table table)
  (let ((filtered-table (remove-from-symbol-table table "last_fn")))
    (if (null? filtered-table)
        (printf "No functions stored\n")
        (begin
          (printf "Number of functions: ~a\n" (length filtered-table))
          (for-each (lambda (pair)
                      (printf "~a (lambda (x) ~a)\n" (car pair) (cdr pair)))
                    filtered-table)))))

;; display the temperatures.
(define (display-data samples)
  (if (null? samples)
      (printf "No samples stored\n")
      (begin
        (printf "Number of samples: ~a\n" (length samples))
        (for-each (lambda (x) (printf "~a\n" (format-to-2-decimals x)))
                  (reverse samples))
        (printf "Average value: ~a\n"
                (format-to-2-decimals (/ (apply + samples) (length samples)))))))

;; Make each number go to two decimal places, like the numbers in the given output files.
;; num parameter is the number that will be formatted.
(define (format-to-2-decimals num)
  (let* ((rounded (/ (round (* num 100)) 100.0))
         (formatted (number->string rounded)))
    (if (string-contains? formatted ".")
        (let ((decimal-parts (string-split formatted ".")))
          (string-append (first decimal-parts) "."
                         (if (= (string-length (second decimal-parts)) 1)
                             (string-append (second decimal-parts) "0")
                             (second decimal-parts))))
        (string-append formatted ".00"))))

;; display when finished. (when in the final state)
(define (display-final-state state)
  (match state
    [(list samples _ _)
     (display-data samples)]))

;; start
(read-input-and-process)
