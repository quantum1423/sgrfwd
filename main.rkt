#lang racket
(require sagiri-comm
         natrium-crypt)

(define server-fwd '())
(define server-keys (make-hash))
(define client-fwd '())

(command-line
 #:program "sgrfwd"
 #:multi
 ("--client" fwdln
             "Add a client-side port mapping. Example: 55555:10-0-0-1--1fdf21....aa42.edge.sagiri:12345"
             (set! client-fwd (cons fwdln client-fwd)))
 ("--server" fwdln
             "Add a server-side port mapping. Example: 22:12345"
             (set! server-fwd (cons fwdln server-fwd)))
 #:args ()
 #f)

(define (forward-socket in out rin rout)
  (dynamic-wind
   void
   (lambda()
     (thread
      (lambda()
        (dynamic-wind
         void
         (lambda()
           (copy-port in rout))
         (lambda()
           (close-input-port in)
           (close-output-port rout)))))
     (copy-port rin out))
   (lambda()
     (close-input-port rin)
     (close-output-port out))))

(dynamic-wind
 void
 (lambda()
   ;; spin off a thread for every client-side port mapping
   (for ([fwdln client-fwd])
     (match (string-split fwdln ":")
       [(list my-port their-host their-port)
        (set! my-port (string->number my-port))
        (set! their-port (string->number their-port))
        (thread
         (lambda()
           (define listener (tcp-listen my-port
                                        256
                                        #t
                                        "127.0.0.1"))
           (for ([i (in-naturals)])
             (printf "waiting\n")
             (define-values (in out) (tcp-accept listener))
             (printf "accepted\n")
             (thread
              (lambda()
                (dynamic-wind
                 void
                 (lambda()
                   (printf "about to sagiri-connect\n")
                   (define-values (rin rout)
                     (sagiri-connect their-host their-port))
                   (printf "sagiri-connect returned\n")
                   (forward-socket in out rin rout))
                 (lambda()
                   (close-input-port in)
                   (close-output-port out))))))))]))
   ;; add the server port mappings to the table
   (for ([fwdln server-fwd])
     (match (string-split fwdln ":")
       [(list int-port ext-port)
        (set! int-port (string->number int-port))
        (set! ext-port (string->number ext-port))
        (define-values (priv _) (eddsa-generate-keys))
        (hash-set! server-keys ext-port priv)
        (define-values (host port)
          (sagiri-start-forward #:private-key priv
                                #:external-port ext-port
                                #:internal-port int-port))
        (printf "Server forwarding ~a:~a to port ~a\n" host port int-port)]))

   ;; do nothing
   (for ([i (in-naturals)])
     (sleep i)))
 (lambda()
   ;; remove all the mappings
   (for ([fwdln server-fwd])
     (match (string-split fwdln ":")
       [(list int-port ext-port)
        (set! int-port (string->number int-port))
        (set! ext-port (string->number ext-port))
        (printf "Removing server mapping on external port ~a\n" ext-port)
        (sagiri-stop-forward #:private-key (hash-ref server-keys ext-port)
                             #:external-port ext-port)]))))