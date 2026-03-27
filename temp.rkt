;; A list-of-temp is either:
;;   - empty
;;   - (cons num list-of-temp)
;; where each num represents a temperature in Fahrenheit

(define temp-list1 (list -40 32 50 68 77))
(define temp-list2 (list 50 32 20 -4 68))



;; freezing-temps : list-of-temp -> list-of-temp
;; Takes in a list of tempratures in Fahernheit and
;; Outputs all of the freezing temps, 32 and below
(define (freezing-temps temps)
  (cond
    [(empty? temps) empty]
    [(> (car temps) 32) (freezing-temps (cdr temps))]
    [else (cons (car temps) (freezing-temps (cdr temps)))]))

(check-expect (freezing-temps temp-list1) (list -40 32))
(check-expect (freezing-temps temp-list2) (list 32 20 -4))

;; to-celsius : list-of-temp -> list of nums
;; Converts of the list-of-temp from Fahernheit to a new
;; list now converted in celsius
(define (to-celsius temps)
  (cond
    [(empty? temps) empty]
    [else (cons (* (- (car temps) 32) (/ 5 9)) (to-celsius (cdr temps)))]))

(check-expect (to-celsius temp-list1) (list -40 0 10 20 25))
(check-expect (to-celsius (list 32 212)) (list 0 100))


;; any-negative : list-of-temp -> bool
;; Determines whether there is a temprature in the list
;; below 0 degrees Fahernheit
(define (any-negative? temps)
  (cond
    [(empty? temps) #false]
    [(>= (car temps) 0) (any-negative? (cdr temps))]
    [else #true]))

(check-expect (any-negative? (list 50 32)) #false)
(check-expect (any-negative? temp-list1) #true)
(check-expect (any-negative? temp-list2) #true)


#|
This was fun quiz to do, Mr. K/Claude,
I am now going to make this in C++ if I feel like it later :)

Thursday, March 26th, 2026
|#
