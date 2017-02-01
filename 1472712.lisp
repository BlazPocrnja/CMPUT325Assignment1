;CMPUT 325 Assignment 1
;Author: Blaz Pocrnja
;Student ID: 1472712

#| Question 1.

The function xmember returns T if argument Y is a member of list X and returns NIL otherwise. Can test for 
lists being members of lists, and both X and Y can be NIL or be a list containing NIL. 

If X is empty, xmember returns NIL, otherwise it checks if the first element is equal to Y.
If it is, T is returned, otherwise xmember is called on the rest of the list excluding the first element.

Examples:
(xmember '(1) '1)				=>T
(xmember '((1) 2 3) '1)			=>NIL
(xmember '((1) 2 3) '(1))		=>T
(xmember nil nil)				=>NIL
(xmember '(nil) nil)			=>T
(xmember '((nil)) nil)			=>NIL
(xmember '(1 2 3 (nil)) '(nil))	=>T
(xmember '(nil) '(nil))			=>NIL

|#
(defun xmember (X Y)
	(cond 
		((null X) nil)
		((equal (car X) Y) t)
		(t (xmember (cdr X) Y))
	)
)


#| Question 2.

The function flatten takes the argument x as a list with possible sublists nested at any depth,
and returns a list of the atoms appearing in x in the same order.

If the first element of x is an atom, then a new list is constructed with that atom and the rest of the list
"flattened". Otherwise the first element is "flattened" and appended to the rest of the list which is also 
"flattened".

Examples:
(flatten '(a (b c) d))				=>(a b c d)
(flatten '((((a)))))				=>(a)
(flatten '(a (b c) (d ((e)) f)))	=>(a b c d e f)

|#
(defun flatten (x)
	(cond 
		((null x) nil)
		((atom (car x)) (cons (car x) (flatten (cdr x))))
		(t (append (flatten (car x)) (flatten (cdr x))))
	)
)


#| Question 3.

The function mix takes two lists L1 and L2, and mixes the elements into a single list by choosing elements
from L1 and then L2 alternatingly. If one list is shorter than the other, then all elements from 
the longer list are appended at the end.

If either list is null, the non null list is returned. Otherwise the first elements from L1 and L2 
are made into a list and appended to the "mixed" list with the rest of L2 and L1 as inputs respectively.

Examples:
(mix '(d e f) '(a b c))				=>(a d b e c f)
(mix '(a) '(1 2 3))					=>(1 a 2 3)
(mix '(d e f g h) '((a) (b c))) 	=>((a) d (b c) e f g h)
(mix nil '(1 2 3))					=>(1 2 3)
(mix '(nil) '(1 2 3))				=>(1 nil 2 3)

|#
(defun mix (L2 L1)
	(cond
		((null L2) L1)
		((null L1) L2)
		(t (append (list (car L1)(car L2)) (mix (cdr L2)(cdr L1))))
	)
)


#| Question 4.

The function split takes a list L as an argument, and puts each element of L into sublists L1 and L2 
alternatingly. i.e (L) ---> (L1 L2)

If L is empty, the function returns (nil nil), otherwise the first element is constructed into L1
and the second element is constructed into L2. This pattern continues by splitting the rest of the
list after the second element. 

Examples:
(split '(1 2 3 4 5 6))				=>((1 3 5) (2 4 6))
(split '((a) (b c) (d e f) g h)) 	=>(((a) (d e f) h) ((b c) g))
(split '())							=>(nil nil)

|#
(defun split (L)
	(if (null L) 
		(list nil nil)
		(list (my_cons (car L) (car (split (cddr L))))
			(my_cons (cadr L) (cadr (split (cddr L)))) 
		)
	)
)

#|

The helper function my_cons is used in the function split to prevent the case where cons returns (NIL) 
when given NIL for both X and Y inputs. It does this by checking if both arguments are NIL, if they are 
then NIL is returned, otherwise cons is called as usual. 

Example:
Without my_cons the second example for split above outputs this.

(split '((a) (b c) (d e f) g h)) 	=>(((a) (d e f) h) ((b c) g NIL))

Which is similiar, but L2 has an extra NIL element at the end.

|#
(defun my_cons (X Y)
	(if (and (null X) (null Y))
		nil
		(cons X Y)
	)
)


#| Question 5.

5.1.
(split (mix L2 L1)) does NOT always return (L1 L2).

If L2 has one more element than L1...
Examples:
(split (mix '(a) '()))			=>((a) NIL)
(split (mix '(a b) '(1)))		=>((1 b) (a))
etc...

or if L1 has two more elements than L2...
Examples:
(split (mix '() '(1 2))) 			=>((1) (2))
(split (mix '(a) '(1 2 3))) 		=>((1 2) (a 3))

then (L1 L2) is not returned.

5.2.
(mix (cadr (split L)) (car (split L))) does NOT return L if the one of the last two elements is NIL.

Examples:
L = (NIL)
(mix (cadr (split '(NIL))) (car (split '(NIL)))) 					=>NIL

L = (NIL NIL)
(mix (cadr (split '(NIL NIL))) (car (split '(NIL NIL)))) 			=>NIL

L = (1 NIL)
(mix (cadr (split '(1 NIL))) (car (split '(1 NIL)))) 				=>(1)

L = (1 NIL NIL)
(mix (cadr (split '(1 NIL NIL))) (car (split '(1 NIL NIL)))) 		=>(1)

L = (1 NIL 2)
(mix (cadr (split '(1 NIL 2))) (car (split '(1 NIL 2)))) 			=>(1 2)

L = (1 2 NIL)
(mix (cadr (split '(1 2 NIL))) (car (split '(1 2 NIL)))) 			=>(1 2)

|#


#| Question 6.

The function subsetsum takes a sum S and a list L as arguments, and returns a subset of L that sums up to S.

First the given list L is copied into sorted_L and sorted from lowest to highest. The function sorted_subsetsum 
is called using S and sorted_L as arguments, and returns the subset of sorted_L that sums up to S if it exists. 
This subset, subset_sol, is intersected with the original list L to return the subset in its original unsorted order.

How sorted_subsetsum and intersect work will be described in detail below.

Examples:
(subsetsum 5 '(1 2 3))					=>(2 3)
(subsetsum 2 '(1 5 3))					=>nil
(subsetsum 29 '(1 16 2 8 4))			=>(1 16 8 4)
(subsetsum 10 '(1 1 5 6 8))				=>(1 1 8)
(subsetsum 5 '(1 10 100 1000 10000))	=>nil

|#
(defun subsetsum (S L)
	(let* 
		(
			(sorted_L (sort (copy-list L) #'<))
			(sorted_sol (sorted_subsetsum S sorted_L))
		)
			(intersect L sorted_sol)
	)
)

#|

The function sorted_subsetsum takes a sum S and a sorted list L from lowest to highest, and returns a subset of L
that sums up to S.

It works using the following recursion formula:

	Base Cases: L is empty. Return NIL.
				The lowest value (i.e the first in L) is larger than S, then a subset is not possible. Return NIL.
				The lowest value is equal to S. No need to check the rest of the list, this value IS a subset sum. Return (list (car L)).

	Rest Cases: Check if the first element is part of the subset...
				If the rest of L and S-first returns a subset, then the first element must be part of the solution, add it to the returned subset.
				Else if the rest of L and S returns a subset, then return that without the first element in L.
				Otherwise there is NO solution possible, return NIL.

|#
(defun sorted_subsetsum (S L)
	(cond 
		((null L) nil)
		((> (car L) S) nil)
		((= (car L) S) (list (car L)))
		(t	(let 
				(
					(with_first (sorted_subsetsum (- S (car L)) (cdr L)))
					(without_first (sorted_subsetsum S (cdr L)))
				)

				(cond
					((not (null with_first)) (cons (car L) with_first))
					((not (null without_first)) without_first)
					(t nil)
				)

			)
		)
	)
)

#|
The function intersect takes two lists L1,L2 and returns the intersection between the two.

If L1 or L2 is empty, then there is no intersection. 
If the first element of L1 is a member of L2 then add it to the intersect of the rest of L1, and L2.
Otherwise just intersect the rest of L1, and L2.

|#
(defun intersect (L1 L2)
	(cond
		((or (null L1) (null L2)) nil)
		((xmember L2 (car L1)) (cons (car L1) (intersect (cdr L1) L2)))
		(t (intersect (cdr L1) L2))
	)
)


