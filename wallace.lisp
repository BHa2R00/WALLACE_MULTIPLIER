
(defun bit-g1 (a1 b1)
  (list 'and2 a1 b1))

(defun bit-p1 (a1 b1)
  (list 'xor2 a1 b1))

(defun bit-g10 (c p1 g1)
  ;(list 'or2 (list 'and2 c p1) g1)
  (list 'ao21 c p1 g1)
  )

(defun bit-s1 (c0 p1)
  (list 'xor2 c0 p1))

(defun bit-add (a b &optional c)
  (if c 
	(bit-s1 c (bit-p1 a b))
	(bit-p1 a b)))

(defun bit-co (a b &optional c)
  (if c
	(bit-g10 c (bit-p1 a b) (bit-g1 a b))
	(bit-g1 a b)))

(defun allocate-ripple-adder (&key a b s ci co)
  (let ((c (list)))
	(seta (car s) (if ci (bit-add (car a) (car b) ci) (bit-add (car a) (car b))))
	(setf c (if ci (bit-co (car a) (car b) ci) (bit-co (car a) (car b))))
	(map 'list
		 (lambda (s-bit a-bit b-bit)
		   (seta s-bit (bit-add a-bit b-bit c))
		   (setf c (bit-co a-bit b-bit c)))
		 (cdr s) (cdr a) (cdr b))
	(if co (seta co c))
	s))

(defun wallace-place-adder-line (l)
  (if (and 
		(>= (length l) 3) 
		(listp (car l))
		(equalp (car (car l)) 'bit-co)
		)
	(append (list (append (list 'bit-add) (reverse (subseq l 0 3)))) (subseq l 3))
	(append (list (append (list 'bit-add) (reverse (subseq l 0 2)))) (subseq l 2))
	))

(defun wallace-co-line (l)
  (if (and
		(>= (length l) 3)
		(listp (car l))
		(equalp (car (car l)) 'bit-co)
		)
	(append (list 'bit-co) (reverse (subseq l 0 3)))
	(append (list 'bit-co) (reverse (subseq l 0 2)))
	))

(defun wallace-expand-adder (w)
  (cond
	((atom w) w)
	((and
	   (listp w)
	   (equalp (car w) 'bit-co)
	   )
	 (wallace-expand-adder
	   (apply 'bit-co (cdr w))))
	((and
	   (listp w)
	   (equalp (car w) 'bit-add)
	   )
	 (wallace-expand-adder
	   (apply 'bit-add (cdr w))))
	(t (mapcar 'wallace-expand-adder w))))

(defun wallace-place-adder (w k)
  (let ((w1 w)
		(c (list)))
	(setf w1
		  (map 'list
			   (lambda (l)
				 (cond
				   ((and 
					  c 
					  (< (length l) (- k 1))
					  )
					(let ((c1 c))
					  (setf c (list))
					  (append (list c1) l)))
				   ((and
					  c 
					  (>= (length l) (- k 1))
					  )
					(let ((c1 c))
					  (setf c (wallace-co-line l))
					  (append (list c1) (wallace-place-adder-line l))))
				   ((>= (length l) k)
					(let ()
					  (setf c (wallace-co-line l))
					  (wallace-place-adder-line l)))
				   (t l)))
			   w1))
	w1))

(defun wallace-depth (w)
  (let ((n 0))
	(map 'list
		 (lambda (l)
		   (let ((m (length l)))
			 (if (> m n)
			   (setf n m))))
		 w)
	n))

(defun wallace-compress (w)
  (let ((d (wallace-depth w))
		(w1 w))
	(do ((n d (- n 1)))
	  ((= n 1))
	  (setf w1 (wallace-place-adder w1 n)))
	w1))

(defun wallace-reform (w)
  (let ((w1 w))
	(setf (nth 0 w1) (list (car (nth 0 w1)) *VSS*))
	(setf (nth 1 w1) (list (nth 1 (car (nth 1 w1))) (nth 2 (car (nth 1 w1)))))
	w1))

(defun allocate-wallace-multiplier (&key m a b)
  (let ((w (list))
		(a1 (list))
		(b1 (list)))
	(dotimes (k (- (length m) 1))
	  (let ((l (list)))
		(dotimes (i (length a))
		  (dotimes (j (length b))
			(if (= (+ i j) k)
			  (push  
				(list 'and2 (nth i a) (nth j b))
				l))))
		(push (reverse l) w)))
	(setf w (reverse w))
	(setf w (wallace-compress w))
	(setf w (wallace-reform w))
	;(format t "w:~%~S~%~%" w)
	(setf w (wallace-expand-adder w))
	(setf a1 (mapcar 'car w))
	(setf b1 (mapcar 'cadr w))
	(allocate-ripple-adder
	  :s (subseq m 0 (- (length (append a b)) 1))
	  :a a1
	  :b b1
	  :co (nth (- (length (append a b)) 1) m)
	  )
	m))
