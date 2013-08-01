; a)
; Peter->Paul->Mary
; (100 + 10 - 20) / 2
; 45
;
; Peter->Mary->Paul
; ((100 + 10)/2 - 20)
; 35
;
; Paul->Mary->Peter
; (100 - 20)/2 + 10
; 50
;
; Mary->Peter->Paul
; (100/20) + 10 - 20
; 40
;
; b)
; 55
; (Peter,Mary|Paul), execute Peter, Paul concurrently, Paul return,
; then Peter return(which set balance to 110), then Mary
; 30
; (Mary,Paul|Peter)
; First, Mary, then Paul and Peter concurrently, Peter first returns,
; tehn Paul returns.
