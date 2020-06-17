#lang racket

(define  average  (lambda ( a b c ) (  / (+ a b c )  3 ) ) )

(average 4 7 4)

; 2)
(define  evalPoly (lambda ( a b c x) ( + (* x x a) (* x b) c )))
(evalPoly 1 2 3 5)

; 3)

(define gcd (lambda (a b)

  (if (and (equal? a 0) (equal? b 0) )

      ( display '(" no greatest common divisor " ) ) ; if true

      (if (equal? b 0) a           

         (if (equal? a 0) b (if (>= a b)

                  (gcd (- a b) b) (gcd a (- b a) )
                             )
         ) 
      )          )  )  
)


(gcd 72 96)

; 4)
( define f
      ( lambda ( lst)
          (  +
                ( * ( car (car lst)) ( car ( cadr lst) ) )  ; (caar lst) is 3 , (caadr lst) is 6
                 (* (cadr (car lst) )  ( cadr ( cadr lst) ) ) ; (cadar lst) is 5, (cadadr lst) is 9
           )))
( f  '(  (3 5)  ( 6 9)  ) )

; 63 is returned

#| What is returned in general?  Check your answer using Dr Racket. What happens if we give f a list of more than two pairs of numbers?  of one pair?
In general, we get the sum of (the product of the caar and the caadr of the list), 
and (the product of the cadar and cadadr of the list). The answers vary based on different lists.
If we add any pairs of numbers after the first 2 items of the list ((3 5) (6 9)), we still get 63, because the lambda functions has car and cdr’s list functions that deals with just the first 2 items of the list. If we add pairs of numbers different from ((3 5) (6 9)) before the list ((3 5) (6 9)), we get another answer than 63. If we just put one pair of numbers, we will get an error message, because the list is too small for the following lambda expressions, because there are too much car’s and cdr’s for such a small list.    |#


; 5)
  ( define g
      ( lambda ( lst)
           (let  ( [first ( car lst) ]   ; '(3 5 2) 
                    [second (cadr lst ) ] ) ; '(6 9 5)
                    ( +
                           (* (car first) (car second) ) ; 3 * 6 
                           ( * (cadr first) (cadr second) ) ; 5 * 9
                            (* (caddr first) (caddr second) ); 2 * 5
                     )                                       ;18 + 45 + 10 = 73  
)))

 ( g   '(  (3 5  2)  ( 6 9  5) )  )                     

; 73 is returned

; 6)

(define middle (  lambda (a b c) 

   ( cond ((or(and (< b a) (< a c)) (and (< c a) (< a b))
          ) a)
          ((or(and (< a b) (< b c)) (and (< c b) (< b a))
          ) b)  
          ((or(and (< a c) (< c b)) (and (< b c) (< c a))
          ) c)
          (else display "at least 2 numbers are equal, there are no middle numbers" )
          )
) )

(middle 4 10 9)

; 7)

(define DistanceFromOrigin (  lambda (x) ( sqrt(  

               +
               (* (car x) (car x)) 

               (* (cadr x) (cadr x))   
                                 )
                           )
            )
)

 (define greater ( lambda (point1 point2) 

                       ( if ( > (DistanceFromOrigin point1) (DistanceFromOrigin point2))

                            (display "true")

                            (display "false")          
                        )
                                          
                 )
 )

( greater '(10  6)  '( 2  3) )


#| run of the code: first line is for #1, 2nd line for #2 and so on


5
38
24
63
73
9
true
 |#
