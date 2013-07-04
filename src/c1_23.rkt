;; replace the next-gen in prime.rkt from next-1 to next-2 and
;; do the benchmark in c1_22.rkt again. The prog runs two times faster.

#|~/code/sicp/chapter1@seckcoder$ racket c1_22.rkt |#
;100000000003
;100000000019
;100000000057
;cpu time: 47 real time: 46 gc time: 0
;1000000000039
;1000000000061
;1000000000063
;cpu time: 142 real time: 142 gc time: 0
;10000000000037
;10000000000051
;10000000000099
;cpu time: 547 real time: 546 gc time: 0
;100000000000031
;100000000000067
;100000000000097
;cpu time: 1799 real time: 1796 gc time: 0
;"done!\n"
;~/code/sicp/chapter1@seckcoder$ racket c1_22.rkt 
;100000000003
;100000000019
;100000000057
;cpu time: 24 real time: 24 gc time: 0
;1000000000039
;1000000000061
;1000000000063
;cpu time: 76 real time: 75 gc time: 0
;10000000000037
;10000000000051
;10000000000099
;cpu time: 281 real time: 281 gc time: 0
;100000000000031
;100000000000067
;100000000000097
;cpu time: 927 real time: 926 gc time: 0
#|"done!\n"|#
