; It's a waste of time to draw the timing diagram repeatedly.
; The problem with the implementation of test-and-set! is really obvious:
; it's possible for two processes acquire the mutex simutaneously. This is dangerous.
