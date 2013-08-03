; Not right. The essential difference is that 'transfer' didnot need to
; calculate the difference between accounts, so it will not be influenced
; by other procedures that try to modify balance of the from-account/to-account.
; But 'transfer' is not good for real-world application. Since there's no transaction
; guarantee for 'transfer', it's possible that from-account will succeed while to-account
; will never be executed.
