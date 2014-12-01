#lang racket

(require rackunit
         (prefix-in m: "main.rkt"))

(test-case
 "Defer works with no delay"
 (let ([success #f])
   (m:defer (lambda () (set! success #t)))
   (check-true success)))

(test-case
 "Defer works with delay via after"
 (let ([success #f])
   (m:after 1 #:seconds do (set! success #t))
   (m:wait-queue)
   (check-true success)))

(test-case
 "Deferred task isn't run before the specified time"
 (let ([success #f])
   (m:after 100 #:hours do (set! success #t))
   (check-false success)
   ;; remove the task from the queue
   (m:purge-queue)))

(test-case
 "Purging results in an empty queue"
 (m:after 100 #:hours do (displayln "Hello, world!"))
 (let ([queue-length '()]
       [sem (make-semaphore)])
   (m:apply-queue (lambda (queue)
                    (set! queue-length (set-count queue))
                    (semaphore-post sem)))
   (semaphore-wait sem)
   (check-equal? queue-length 1)
   (m:purge-queue)
   (m:apply-queue (lambda (queue)
                    (set! queue-length (set-count queue))
                    (semaphore-post sem)))
   (semaphore-wait sem)
   (check-equal? queue-length 0)))
