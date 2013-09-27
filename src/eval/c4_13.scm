; 1. I don't think we really need `make-unbound`
; 2. If we really need it, then it should be a terrible idea to
; provide a feature that can remove the binding from enclosed environment.
; Since that may bring ugly code. For example, if we call a func wrote by
; others, and the func may remove some binding silently, which will cause
; hard to debug bugs. Another case is, if the program provides some interface
; that can insert code by others, than the malicious users can call the unbound
; feature to bring down the whole system.
