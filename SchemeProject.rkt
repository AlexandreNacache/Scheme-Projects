#lang racket


(define is1? (lambda (tuple)
                       (if (and (= 1 (- (cadr tuple) (car tuple)) )  (= 2 (length tuple) ) (list? tuple) )  

                                  #t
 
                                  #f
                       )
            )
)

(define greenlist? ( lambda (x) 

                         (andmap is1? x)          
                                 
                    )
 )


 (greenlist? '(  ( 4.5  5.5)  (  7  8)  ))   
 (greenlist?    '(  )   )
 (greenlist? '( ( 2 4 6)  ( 5 6) ( 1 2)))      
 (greenlist? '(   ( 5 6)  ( 3  4)  ( 2  3)  ( -5  -4)  ))
 (greenlist? '(  ( 4  7)  ( 9  10 )  ))
 (greenlist? '(  3   4   5   6)  ) 
 (greenlist? "green apples" )
 (greenlist? '(  3   4   5   6)  ) 
 

#| ------ run of the code ----------
#t
#t
#f
#t
#f
-----------------------------------|#



#|
1.   Write a non-recursive Scheme function to check if a string is a palindrome  ( palindrome?  s)
Hint: use string->list and reverse and equal?.  Test your function on the following strings
"aaabccbaaa"
“kayak”
"red der"   -- this is palindrome
"a"
"1234567654321"
“algorithm”
""    -- this is the empty string
|#
; ---------------------------------------------
;  Problem 1

(define palyndrome (lambda (str) 

                                (define rlst (reverse (string->list str)) ) 
                                (define lst  ( string->list str))
                                  (if (equal? rlst lst)

                                      (display "true")
                                      (display "false")
                                  )
                                  
                     )
 )

(palyndrome "aaabccbaaa")
(displayln " ")
(palyndrome "kayak")
(displayln " ")
(palyndrome "red der")
(displayln " ")
(palyndrome "a")
(displayln " ")
(palyndrome "1234567654321")
(displayln " ")
(palyndrome "algorithm")
(displayln " ")
(palyndrome "")

;results bellow.
#|
true 
true 
true 
true 
true 
false 
true
|#


;; ----------------------------------------------------------------


#|
2.   Write a recursive Scheme function to remove duplicates from a list. Should work on lists of
all types. (removeDups lst1)  You will need to use cons since you are creating a new list. Do
not use built-in functions  sort or remove. 
Outline:
|#


(define member (lambda (item list)

                   (cond

                     ((null? list) #f) 
                     
                     ((equal? item (car list)) #t )

                     (else (member item (cdr list)))
                    )
                 )
)
(define removeDups ( lambda ( lst)
                      (cond
                            ( (equal? lst '() ) '() )             ;case  where  lst is empty

                             ((member (car lst) (cdr lst)) (removeDups (cdr lst)) )


                           (else (cons (car lst) (removeDups (cdr lst))) ) 
                           
                       )
                   )                  
)

(removeDups '(  3  5  6  5  7  5  8  9  3  3  9) )
 (removeDups '(  ( 4  6)   (  1  2)   ( 3   1)   (  1  2)   ( 4  6)  (  7  8)  ))



#|
3.   Write a recursive Scheme function  (pairwiseSum lst1 lst2) ) to return the pairwise sums of
two lists of integers. Return an error message if the lists are not the same length. Use cond
and cons.  [Hint:  The base cases occur when lsts of of different sizes or when both lists are
empty]
Examples:
(pairwiseSum  ‘( 3 4 5 6)  ‘( 10 20 32 40)  ) => ‘(13  24  37  46).
(pairwiseSum  ‘( )  ‘(  )  ) => ‘( )
(pairwiseSum  ‘( 4 5) ‘ ( 1 2 3)  )  => “error message”
Test your function on the above examples.
|#



;; ---------------------------------------------
;;Problem 3
(define pairwiseSum  (lambda ( list1 list2)

                          (cond

              ;base case 1
                            ( (not(= (length list2)(length list1))) (display "error, both lists must be the same length")  ) 

                            
              ;base case 2

                           ( (and (null? list1) (null? list2)) '()   ) 

 
                            ((= (length list1) (length list2)) (cons (+ (car list1) (car list2))(pairwiseSum(cdr list1) (cdr list2))
                                                                )
                            )

                            (else (display "error"))
                           )
                     )  
)
(pairwiseSum  '( 3 4 5 6)  '( 10 20 32 40)  )
(pairwiseSum  '( )  '(  )  )
(pairwiseSum  '( 4 5) '( 1 2 3)  ) 

; results
#|
'(13 24 37 46)
'()
error, both lists must be the same length
> 
|#
;; ----------------------------------------------------------------

#|
4.   Write a recursive Scheme function (makelist n item) that creates a list of length n, initialized
with n copies of item. For example,  (makelist 5  "red") =>  ‘( "red"   "red"   "red"  "red"  
"red"  )
Test your function on (makelist 8   “blue” )  and  ( makelist  6   ‘( 0  0 )  )  [Hint: The base
case is n = 0]
|#
;; ---------------------------------------------
;;Problem 4

     ( define makelist (lambda (n item)

                               ( cond

                                   ((= n 0) '())
                             
                                   ((< n 0)(display "positive #s only"))

                                   (else  (cons item (makelist (- n 1) item )    )
                                    )

                                )
                             )
         )

( makelist  6   '( 0  0 )  )
(makelist 8   "blue" )


;;  test results for problem 4


;  '((0 0) (0 0) (0 0) (0 0) (0 0) (0 0))
;  '("blue" "blue" "blue" "blue" "blue" "blue" "blue" "blue") 

;; ----------------------------------------------------------------


#|
5.   Write a non-recursive Scheme function (rotate lst ) that returns a rotated list by moving the
first element to the end of the list. Check that the input argument is a list.  Return error
message if not.
[Hint: use append]
Example:
(rotate  ‘( a b c d e) ) ) should return  ‘( b c d e a)
(rotate  ‘( "red"   "blue")  )  => ‘(  "blue" "red")
(rotate  ‘( ) ) => ‘( )
( rotate  "abc" )  =>  “not a list”
Test your function on all of the above examples.
|#



;; ---------------------------------------------
;;Problem 5

(define rotate (lambda (lst) 

                          (cond
                                ((null? lst) '()) 

                                ((list? lst) ( append (cdr lst) (cons (car lst) '()))  
                                )            
 
                               (else (display "not a list"))
                           )
                  )
   )

(rotate  '( a b c d e) ) 
(rotate  '( "red"   "blue")  ) 
(rotate  '( ) )
( rotate  "abc" )

#|
  test results for problem 4
  '(b c d e a)
  '("blue" "red")
  '()
   not a list
|#
;; ----------------------------------------------------------------

#|
6.   Challenge:  Create a recursive Scheme function , ( nrotations  lst  n)  , that creates a list of the first n rotations  of a given list. Use rotate from Problem 7. 
Examples:

(nrotations   ‘( 1 2 3 )   3)  =>   ‘( ( 2 3 1)  ( 3 1 2)  ( 1 2 3) )
 (nrotations   ‘(  s  t  o  p )   5)  =>  ‘( ( t o p s)  ( o p s t)  ( p s t o)   ( s t o p)  ( t o p s)  )
(nrotations  lst  0 )  =>  ‘()
|#

;; ---------------------------------------------
;;Problem 6

(define rotate (lambda (lst) 

                          (cond
                                ((null? lst) '()) 

                                ((list? lst) ( append (cdr lst) (cons (car lst) '()))  
                                )            
 
                               (else (display "not a list"))
                           )
                  )
   )

  (define  nrotations (lambda (lst n)

                           (cond

                                ((= n 0) '())

                                ((> n 0) (cons (rotate lst) (nrotations (rotate lst) (- n 1)) )) 
                                
                                (else (display "enter positive # only")
                                 )
                           )
                       )
)
    
 (nrotations   '( 1 2 3 )   3)  
 (nrotations   '(  s  t  o  p )   5)  

#|  test results for problem 6

'((2 3 1) (3 1 2) (1 2 3))
'((t o p s) (o p s t) (p s t o) (s t o p) (t o p s))

|#
;; ----------------------------------------------------------------


#|

7.   A greenlist is a non-empty list of pairs of integers  where a pair of integers is a list of
exactly two integer and where each pair ‘( x y)  has the property that y – x  = 1. 
Example:  ‘(  ( 5 6)  ( 3  4)  ( 2  3)  ( -5  -4)  ) is a greenlist. The following items are not greenlists:
     ‘(  ( 4  7)  ( 9  10 )  )  , 
	 ‘(  ( 4.5  5.5)  (  7  8)  )   , 
	 ‘(  )   ,
      ‘( ( 2 4 6)  ( 5 6) ( 1 2))  ,  
	  ‘(  3   4   5   6)   , 
	  "green apples"
	  
Write a Scheme predicate, ( greenlist?    z) ,  hat returns #t if z is a greenlist; otherwise return
#f. Check ALL conditions. Test your function on all of the above examples.  
[Hint: Create helper functions.  ]


 ‘(  ( 4.5  5.5)  (  7  8)  )   
     ‘(  )   ,
      ‘( ( 2 4 6)  ( 5 6) ( 1 2))      
  ‘(  3   4   5   6)   
    "green apples"
 ‘(  ( 4  7)  ( 9  10 )  )
‘(  ( 5 6)  ( 3  4)  ( 2  3)  ( -5  -4)  )
|#
;  ---------------------------------------------
; Problem 7


(define is1? (lambda (tuple)
                       (if (and (= 1 (- (cadr tuple) (car tuple)) )  (= 2 (length tuple) ) (list? tuple) )  
                                  #t
                                  #f
                       )
            )
)

(define greenlist? ( lambda (x) 

                         (andmap is1? x)          
                                 
                    )
 )

 (greenlist? '(  ( 4.5  5.5)  (  7  8)  ))   
 (greenlist?    '(  )   )
 (greenlist? '( ( 2 4 6)  ( 5 6) ( 1 2)))      
 (greenlist? '(   ( 5 6)  ( 3  4)  ( 2  3)  ( -5  -4)  ))
 (greenlist? '(  ( 4  7)  ( 9  10 )  ))
 (greenlist? '(  3   4   5   6)  ) 
 (greenlist? "green apples" )
 (greenlist? '(  3   4   5   6)  ) 
 

#| ------ run of the code ----------
#t
#t
#f
#t
#f