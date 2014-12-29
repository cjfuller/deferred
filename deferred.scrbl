#lang scribble/manual

@(require (for-label racket
                     "main.rkt"))

@title{Deferred: a simple library for doing things later}

@defmodule[deferred]

Deferred allows you to run things at a specified time or after a specified
elapsed time.  It's sort of like a less complete cron, usable from
within racket.

The central notion within deferred is that of the task queue: a central place
where tasks go to wait.  (This is in fact a misnomer: it's actually just a set,
and tasks handle their own scheduling independently of each other, but I found
it helpful to think about it in this way.)  There's a default queue that
requires no setup, but if you want a bunch of separate taskqueues, you can
achieve this with parameterization.

@section[#:tag "adding"]{Adding a task to a taskqueue}

In deferred, a "task" is just a zero-argument function to be run (which is
internally wrapped into its own thread.  There are several ways to add a
function to a taskqueue, with various levels of convenience.

For running a function at a specified @racket[date], use @racket[defer]:

@defproc[(defer [fn (-> () any)] [#:eta date date? (current-date)])
         void?]{

 Runs the provided function at a later date (defaulting to now). Providing a
date in the past will run the function immediately.  Blocks until the task has
been added to the current taskqueue (which is the value of the
@racket[queue-manager] parameter).  If the taskqueue is no longer accepting
tasks, returns immediately.}

For running a function after a specified interval, use @racket[after]:

@defform[(after num kw body ...)
         #:contracts ([num number?]
                      [kw (one-of/c #:seconds #:minutes #:hours #:days)])]{

 Run the specified body expressions after a given delay.  Only one of the
timing keywords may be provided (this may change in the future).  Uses
@racket[defer] internally, so its specific behaviors apply here as well.}

For convenience, there are shortcuts for running something at a specific hour
and minute today or tomorrow:

@deftogether[(@defform[(today-at hour minute body ...)
                       #:contracts ([hour (</c 24)]
                                    [minute (</c 60)])]
              @defform[(tomorrow-at hour minute body ...)
                       #:contracts ([hour (</c 24)]
                                    [minute (</c 60)])])]{

 Run the specified body expressions at the given time today (or tomorrow).  If
the time has already passed, runs the body expressions immediately.  Uses
@racket[defer] internally, so its specific behaviors apply here as well.
}

@section[#:tag "queue-interaction"]{Interacting with task queues}

Most of the time, it shouldn't really be necessary to interact with the task
queues directly.  Nonetheless, if you want to inspect the contents of a queue,
or override the default queueing behavior in @racket[defer], there are
functions to do so.

@defproc[(enqueue [t thread?])
         bool?]{

Add the specified thread to the tasks tracked by the taskqueue.  (The thread
should already be running.)

Return @racket[#t] if the enqueue message was sent successfully, @racket[#f]
otherwise.}

@defproc[(dequeue [t thread?])
         (or/c void? #f)]{

Remove the specified thread from the tasks tracked by the taskqueue.

Return nothing if the dequeue message was sent successfully, @racket[#f] if the
queue is not running.  Fails silently if the thread is not in the queue.}

@defproc[(inspect-queue)
         (or/c void? #f)]{

@racket[displayln] each of the items in the current queue.  (Each item is a
thread.)}

@defproc[(apply-queue [fn (-> (set/c thread? #:kind 'immutable) any)])
         (or/c void? #f)]{

Apply the provided function to the set of threads in the current queue for
side-effects.}

@defproc[(apply-queue/promise [fn (-> (set/c thread? #:kind 'immutable) any)])
         promise?]{

Apply the provided function to the set of threads in the current queue.
Immediately return a promise that when forced gives the return value of the
function.}

@section[#:tag "queue-shutdown"]{Shutting down a queue}

When you're done with a queue, you can shut it down to remove any pending
tasks, and prevent enqueueing any additional ones.

@defproc[(wait-queue)
         void?]{

 Waits for all tasks in the current queue at the time of the call to finish,
then returns.  Does not prevent additional tasks from being added while
waiting.}

@defproc[(purge-queue)
         (or/c void? #f)]{

 Purges the current queue, immediately killing all tasks that haven't yet
marked themselves as running.  (There's no guard against race conditions. Don't
assume that all tasks that were killed did not run.)

Returns nothing if the purge message was successfully sent or @racket[#f] if
the queue is not running.}

@defproc[(shutdown-queue)
         (or/c void? #f)]{

 Shuts down the current queue, immediately killing all tasks and preventing the
queue from accepting any new ones.  As for @racket[purge-queue], there's no
guarantee about what has and has not yet run.

Returns nothing if the shutdown message was successfully sent or @racket[#f] if
the queue is not running.}

@section[#:tag "custom-queues"]{Making your own task queues}

If you need multiple separate queues, or your own custom queues, you can accomplish this by parameterizing @racket[queue-manager]:

@defparam[queue-manager qm thread? #:value (thread queue-manager-loop)]{

The current task queue used for all other operations.  Basic add/remove
operations are accomplished by sending message @racket['(add item)] and
@racket['(remove item)].  For inspecting the queue and applying a function the
message @racket['(inspect callback-fn)] is used.  For purging and shutting
down, the messages @racket['purge] and @racket['shutdown] are used.  Custom
queue implementations should somehow handle (or ignore) these messages.}

@defproc[(queue-manager-loop [queue-items (set/c any/c #:kind 'immutable) (set)])
         void?]{

A basic queue manager loop, repsonding to @racket['add], @racket['remove],
@racket['inspect], @racket['purge], and @racket['shutdown].  Tracks enqueued
items with an immutable set.  This particular function is agnostic about what
the enqueued items are, though the purge function used for purging/shutting
down will assume they are threads.}

@section[#:tag "util"]{Utility functions}

@defproc[(eta-from-offset [#:seconds sec number? 0]
                          [#:minutes minutes number? 0]
                          [#:hours hours number? 0]
                          [#:days days number? 0])
         date?]{

Get a date corresponding to the suppled offset in seconds, minutes, hours, and
days from now.}
