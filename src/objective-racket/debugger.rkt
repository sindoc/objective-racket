#lang racket

(require "utils.rkt")

(define *initial-dbg-obj-handlers* '())

(define (dbg-obj . args)
  ;; Each debug object handler must extend this abstraction by 
  ;; implementing two messages, i.e. qualifier and formatter. 
  ;; The handler must return a thunk as a response to each message. 
  ;;
  ;; Unknown messages should be delegated back to this abstraction, 
  ;; which is available to specialized debug object handlers as the 
  ;; second argument when they are instantiated. The first argument 
  ;; is the payload that might or might not be supported by one or
  ;; more handlers.
  ;; 
  ;; In case no handler supports a given payload, a default handler 
  ;; is pushed to be used for that payload.
  ;;
  ;; Please use the add-handler! message to add custom handlers. 
  ;; An initial list of handlers is provided by the global variable,
  ;; '*initial-dbg-obj-handlers*', though there's no need to mutate 
  ;; that variable in order to add custom handlers.
  ;;
  ;; One could alter the handler selection policy --since _multiple_
  ;; handlers might qualiy for a given payload-- by providing a 
  ;; custom selector procedure. A priori, there exists a default 
  ;; selector. The selector procedure receives two arguments:
  ;;
  ;; (1) A list of available handlers and,
  ;; (2) A mask list that has as many elements as the first argument.
  ;;
  ;; The values in the second argument indicate whether a particular 
  ;; handler --same position in the first argument-- handles the
  ;; payload that's being processed.
  
  (define self '())
  (define handlers '())
  (define selector '())
  (define payload '())

  (define (init)
    (set! self dispatch-dbg-obj)
    (set! handlers *initial-dbg-obj-handlers*)
    (set! selector default-selector)
    (set! payload (dbg-obj-payload args))
    (cond 
      ((null? args)
       self)
      (else
       (let* 
           ((handler-instances '())
            (handler-qualification-mask
             (map
              (lambda (handler)
                (let ((handler-instance (handler payload self)))
                  (set! handler-instances 
                        (cons handler-instance handler-instances))
                  ((handler-instance 'qualifier))))
              handlers))
            (elected-handler
             (selector 
              (reverse handler-instances) 
              handler-qualification-mask)))
         (cond 
           ((and (not (null? elected-handler))
                 (procedure? elected-handler)
                 (elected-handler 'dbg-obj?))
            elected-handler)
           (else
            (default-dbg-obj payload self)))))))
  
  (define (default-selector handlers handler-qualification-mask)
    ;; Choose the first qualifying handler
    (define selected-handler '())
    (for-each 
     (lambda (handler qualified?)
       (when (true? qualified?)
         (set! selected-handler handler)))
     handlers 
     handler-qualification-mask)
    selected-handler)
  
  (define (selector! new-selector)
    (set! selector new-selector))
  
  (define (add-handler! handler . for-extensibility)
    (when (not (procedure? handler))
      (error "A debug object handler must be a procedure"))
    (set! handlers (cons handler handlers)))
  
  (define (dispatch-dbg-obj msg)
    (case msg
      ((dbg-obj?) #t)
      ((payload) payload)
      ((add-handler!) add-handler!)
      ((handler-selector!) selector!)
      (else
       (error "Unknow message" msg))))
  (init))

(define (dbg-obj-payload payload)
  (define self '())
  (define (init)
    (set! self dispatch-dbg-obj-payload)
    self)
  (define (dispatch-dbg-obj-payload msg)
    (case msg
      ((dbg-obj-payload?) #t)
      ((as-list) payload)
      (else
       (error "Unknown message" msg))))
  (init))

(define (process-dbg-obj-args args)
  (define common-error 
    (string-append 
     "Either 0 or 3 arguments must be provided.\n"
     "If any argument is passed to a debug object, "
     "the argument list MUST have the following structure:\n"
     "an instance of dbg-obj-payload as payload, followed by:\n"
     "an instance of dbg-obj as parent (or usual), followed by:\n"))
  (cond 
    ((and (not (null? args))
          (procedure? (car args))
          ((car args) 'dbg-obj-payload?))
     (let ((payload ((car args) 'as-list)))
       (when (null? (cdr args))
         (error common-error "The second argument is missing.\n"))
       (let ((parent (cadr args)))
         (list payload parent))))
    (else
     (list null null null))))

(define (default-dbg-obj . args)
  (define self '())
  (define payload '())
  (define usual '())
  
  (define (init)
    (let ((structured-args (process-dbg-obj-args args)))
      (set! payload (car structured-args))
      (set! usual (cadr structured-args)))
    (set! self dispatch-default-dbg-obj)
    self)
  
  (define (qualifier) #t)
  (define (formatter)
    (gentext-println payload))
  
  (define (dispatch-default-dbg-obj msg)
    (case msg
      ((default-dbg-obj?) #t)
      ((qualifier) qualifier)
      ((formatter) formatter)
      (else
       (usual msg))))
  (init))

(define (list-proc-dbg-obj . args)
  (define self '())
  (define payload '())
  (define usual '())
  (define lst '())
  (define proc '())
  
  (define (init)
    (let ((structured-args (process-dbg-obj-args args)))
      (set! payload (car structured-args))
      (set! usual (cadr structured-args)))
    (set! self dispatch-list-proc-dbg-obj)
    self)
  
  (define (qualifier)
    (define result
      (and (not (null? payload))
           (not (null? (cdr payload)))
           (list? (car payload))
           (procedure? (cadr payload))))
    (when (true? result)
      (set! lst (car payload))
      (set! proc (cadr payload)))
    result)
    
  (define (formatter)
    (gentext-println (map proc lst)))
  
  (define (dispatch-list-proc-dbg-obj msg)
    (case msg
      ((list-proc-dbg-obj?) #t)
      ((list) lst)
      ((proc) proc)
      ((qualifier) qualifier)
      ((formatter) formatter)
      (else
       (usual msg))))
  (init))


(define (key-val-dbg-obj . args)
  (define self '())
  (define payload '())
  (define usual '())
  (define key '())
  (define val '())
  
  (define (init)
    (let ((structured-args (process-dbg-obj-args args)))
      (set! payload (car structured-args))
      (set! usual (cadr structured-args)))
    (set! self dispatch-key-val-dbg-obj)
    self)
  
  (define (qualifier)
    (define result
      (and (not (null? payload))
           (not (null? (cdr payload)))
           (symbol? (car payload))))
    (when result
      (set! key (car payload))
      (set! val (cadr payload)))
    result)
    
  (define (formatter)
    (gentext-format-key-val-pair key val)
    (gentext-newline))
  
  (define (dispatch-key-val-dbg-obj msg)
    (case msg
      ((key-val-dbg-obj?) #t)
      ((key) key)
      ((value) val)
      ((qualifier) qualifier)
      ((formatter) formatter)
      (else
       (usual msg))))
  (init))

(define (make-debugger level)
  (define range '(0 1))
  (define self '())
  
  (define (init)
    (set! self dispatch-debug)
    self)
  
  (define (level! new-val)
    (when (not (in-range new-val (car range) (last range)))
      (error "Invalid debug level" new-val))
    (set! level new-val))
  
  (define (report context args)
    (unless (zero? level)
      (gentext-start-of-debug-clause context)
      (map
       (lambda (arg)
         (cond
           ((and (procedure? arg)
                 (arg 'dbg-obj?))
            ((arg 'formatter)))
           (else
            (gentext-println arg))))
       args)
      (gentext-end-of-debug-clause)))
  
  (define (dispatch-debug msg . args)
    (case msg
      ((level) level)
      ((level!) level!)
      (else
       (report msg args))))
  (init))

(define (gentext-start-of-debug-clause context)
  (gentext-fancy-line)
  (gentext-println  "Start of debug clause")
  (gentext-println  "Context: " context)
  (gentext-fancy-line))

(define (gentext-end-of-debug-clause)
  (gentext-fancy-line)
  (gentext-println  "End of debug clause"))

;; Initialization
;; ------------------------------------------------------------------------

(set! 
 *initial-dbg-obj-handlers*
 (list 
  key-val-dbg-obj
  list-proc-dbg-obj))

;; Exported Abstractions
;; ------------------------------------------------------------------------
(provide
 make-debugger
 dbg-obj
 default-dbg-obj
 key-val-dbg-obj
 list-proc-dbg-obj
 process-dbg-obj-args)