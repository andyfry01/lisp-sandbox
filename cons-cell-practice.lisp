;; a set of cons cells that doesn't have nil as its final value 
;;this creates a dotted list: (1 2 . 3)
(print (cons 1 (cons 2 3)))

;; if we wanted to print the above list as a "normal" list we'd do this:
(print (cons 1 (cons 2 (cons 3 nil))))

;; a dotted list is Lisp's way of saying "sorry, i tried to print that
;; thing you told me to print, but the final element in the chain of
;; conses wasn't a nil, it was something else 

;; a dotted list can also be thought of as shorthand for cons 
;; this will create a normal list: (1 2 3)
(print '(1 . (2 . (3 . nil))))

;; you can use a dotted list to represent a tuple!  which can be used
;; for pairs, or keys/values, etc.  
;; here is a tuple: 
(print (cons 2 3))
;; prints (2 . 3)

;; you can use the usual car/cdr stuff to access stuff in a tuple
(print (car (cons 2 3)))
;; prints 2

;; you can also create circular lists by setting the final value of the
;; final cons cell to the first value of the whole chain of cons cells:

;; this prevents endless loop problems
(setf *print-circle* t)

(defparameter foo '(1 2 3)) (setf (cdddr foo) foo) (print foo)

;; You can create association lists too! Which is like an .. object kind
;; of thing?  Looks like it's comprised of multiple tuples where the
;; cons is the key and the cdr is the value
(defparameter *drink-order* '((bill . espresso) (lisa . drip-coffee)
						(ted . chai-latte)))

;; assoc will look for the first item in the alist matching the first
;; argument and give you the cdr of the matching tuple
(print (assoc 'lisa *drink-order*))

;; list mutation! we hates it. **gollum**
(push '(lisa . orange-mocha-frappuchino) *drink-order*)

;; now the first item with lisa as a car is a different tuple, the same
;; assoc command will print differently now
(print (assoc 'lisa *drink-order*))

;; you can build tree structures with alists
(defparameter *house* '((walls (mortar (cement) 
				       (water) 
				       (sand))
			       (bricks)) 
			(windows (glass) 
				 (frame)
				 (curtains)) 
			(roof (shingles) 
			      (chimney))))
(print *house*)
;; interesting that when you want to separate the contents of an alist
;; from the key (or title) of the list or sublist, you can wrap
;; individual items in parens. Kind of like how I use parens in JS to
;; evaluate things separately from each other. It all occupies the same
;; list, but is interpreted differently by means of parens. 


