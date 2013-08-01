(library
  (concurrency)
  (export parallel-execute)
  (import (chezscheme))

  (define (parallel-execute . args)
    (for-each fork-thread args))
  )
