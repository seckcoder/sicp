; Another wtf question.
; An extra frame is created since let is evaluated to lambda.
; This question seems forgets an important assumption, that is,
; the internal defintion may be interwined with other expressions.
; In this case, what you need to do is just to reorder the internal
; definitions, which will not cause extra frame construction.
