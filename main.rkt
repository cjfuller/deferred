#lang racket

(require racket/date)

(provide defer
         eta-from-offset
         after
         enqueue
         dequeue
         purge-queue
         shutdown-queue
         inspect-queue
         today-at
         tomorrow-at)

(define (purge items)
  (for ([t (in-set items)])
    (kill-thread t)))

(define (queue-manager-loop [items (set)])
  (let ([message (thread-receive)])
    (match message
      [(list 'add item) (queue-manager-loop (set-add items item))]
      [(list 'remove item) (queue-manager-loop (set-remove items item))]
      [(list 'inspect callback) (begin
                                  (callback items)
                                  (queue-manager-loop items))]
      ['purge (begin
                (purge items)
                (queue-manager-loop))]
      ['shutdown (purge items)])))

(define queue-manager (make-parameter (thread queue-manager-loop)))

(define (enqueue t)
  (void? (thread-send (queue-manager) `(add ,t) #f)))

(define (dequeue t)
  (thread-send (queue-manager) `(remove ,t) #f))

(define (inspect-queue)
  (thread-send (queue-manager) `(inspect ,(lambda (items) (displayln items)))))

(define (purge-queue)
  (thread-send (queue-manager) 'purge #f))

(define (shutdown-queue)
  (thread-send (queue-manager) 'shutdown #f))

(define (defer fn #:eta [eta-date (current-date)])
  (thread (lambda ()
            (when (enqueue (current-thread))
              (sync (alarm-evt (* 1000 (date->seconds eta-date))))
              (dequeue (current-thread))
              (fn)))))


(define (eta-from-offset #:seconds [sec 0] #:minutes [minutes 0] #:hours [hours 0] #:days [days 0])
  (seconds->date (+ (current-seconds)
                    sec
                    (* 60 minutes)
                    (* 60 60 hours)
                    (* 60 60 24 days))))

(define (set-from-base base-time hr mins)
  (struct-copy date base-time
               (hour hr)
               (minute mins)
               (second 0)))

(define-syntax (after stx)
  (syntax-case stx (do)
    [(after num kw do body ...)
     #'(let ([eta (eta-from-offset kw num)])
         (defer (lambda () body ...) #:eta eta))]))

(define-syntax (tomorrow-at stx)
  (syntax-case stx (do)
    [(tomorrow-at hr mins do body ...)
     #'(let ([base-time (eta-from-offset #:hours 24)])
         (defer (lambda () body ...)
           #:eta (set-from-base base-time hr mins)))]))

(define-syntax (today-at stx)
  (syntax-case stx (do)
    [(today-at hr mins do body ...)
     #'(defer (lambda () body ...)
         #:eta (set-from-base (current-date) hr mins))]))


