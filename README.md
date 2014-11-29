# Deferred: a simple time-deferred task library for racket.

## Examples:

Print "Hello, world!" at 8:00 am today:
```racket
(today-at 8 0 do
          (displayln "Hello, world!"))
```

Do the same at 8:00 am tomorrow:
```racket
(tomorrow-at 8 0 do
             (displayln "Hello, world of tomorrow!"))
```

Print "Hello, world!" after 3 hours:
```racket
(after 3 #:hours do
       (displayln "Hello, world!"))
```

Defer running a function until a specified date:
```racket

(require racket/date)

(define my-date (seconds->date (+ (current-seconds)
                                  3600)))

(defer (lambda () (displayln "Hello, world!"))
       #:eta my-date)
```
