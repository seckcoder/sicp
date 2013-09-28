; a) in eval.scm
; b) in eval.scm
; c) if put in make-procedure, than scan-out-defines
; is evaluated when procedure defined. But when put
; in procedure-body, scan-out-defines is evaluated when
; procedure evaluated. It's just a matter of whether
; we want the internal definition to be lazy.
; Note when implement scan-out-defines, to pass
; a (quote symbol) to the evaluator, you have to
; pass (quote (quote symbol)), see my explanation in eval.scm
