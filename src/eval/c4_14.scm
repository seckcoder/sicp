; assume map as: (map proc list-of-args)
; if map is made as builtin, when evaluating map, we
; have the primitive map, but when we evaluate the primitive
; map, we are actuallly interpret it in the interpretor environment,
; and in this environment, proc is not defined.
;
; Therefore, builtin is not allowed to take procedure as arguments.
;
; Note, when we eval map, we should make sure list-of-args is really 
; a list for the interpretor. Below is a demo
;
(load "eval.scm")
(define (simple-test)
  (seck-eval '(define (foo x)
                x)
             global-env)
  (display (seck-eval '(map foo '(1 2 3))  ; make sure (1 2 3) is quoted
                      global-env))
  )
