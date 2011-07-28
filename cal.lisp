;; calendar.pdf dershowitz and reingold 1990
(declaim (optimize (debug 3) (safety 3) (speed 0)))
(sb-ext:restrict-compiler-policy 'debug 3)
#+nil
(sb-ext:describe-compiler-policy)

;; date is a list (list year month day)
(defun make-date (&key (year 1) (month 1) (day 1))
  (declare (type integer year)
	   (type (integer 1 12) month)
	   (type (integer 1 31) day))
  (list year month day 'gregorian))

(defun extract-year (date)
  (assert (gregorian-p date))
  (first date))

(defun extract-month (date)
  (assert (gregorian-p date))
  (second date))

(defun extract-day (date)
  (assert (gregorian-p date))
  (third date))

(defun gregorian-p (date)
  (eq 'gregorian (fourth date)))

(defun make-iso-date (&key (year 1) (week 1) (day 1))
  (declare (type integer year)
	   (type (integer 1 53) week)
	   (type (integer 1 7) day))
  (list year week day 'iso))

(defun iso-p (date)
  (eq 'iso (fourth date)))


(defun extract-iso-year (date)
  (assert (iso-p date))
  (first date))

(defun extract-iso-week (date)
  (assert (iso-p date))
  (second date))

(defun extract-iso-day (date)
  (assert (iso-p date))
  (third date))



(defmacro sum (expression index initial condition)
  `(loop for ,index from ,initial while ,condition sum
	,expression))

#+nil
(sum i i 0 (< i 10))

(defun last-day-of-gregorian-month (month year)
  (declare (type (integer 1 12) month)
	   (type integer year)
	   (values (member 28 29 30 31) &optional))
  (if (and (= month 2)
	   (= (mod year 4) 0)
	   (not (member (mod year 400)
			(list 100 200 300))))
      29 ;; february in leap year
      (nth (1- month)
	   (list 31 28 31 30 31 30 31 31 30 31 30 31))))

(defun absolute-from-gregorian (date)
  (declare (type cons date))
  (assert (gregorian-p date))
  (let* ((month (extract-month date))
	 (year (extract-year date)))
    (declare (type (integer 1 12) month)
	     (type integer year))
    (+ (extract-day date) ;; days so far in this month
       (sum ;; days in prior months this year
	(last-day-of-gregorian-month m year)
	m 1 (< m month))
       (* 365 (1- year)) ;; days in prior years
       (floor (1- year) 4) ;; julian leap days in prior years
       (- (floor (1- year) 100)) ;; minus prior century years
       (floor (1- year) 400) ;; plus prior years divisible by 400
       )))

#+nil
(absolute-from-gregorian (make-date :year 1 :month 3 :day 1))

(defun gregorian-from-absolute (date)
  (declare (type integer date))
  (let* ((approx (floor date 366)) ;; approximation from below
	 (year (+ approx ;; search forward from approximation
		  (sum 1 y approx
		       (<= (absolute-from-gregorian
			    (make-date :year (1+ y)))
			   date))))
	 (month
	  (1+ (sum 1 m 1 ;; search forward from january
		   (< (absolute-from-gregorian
		       (make-date 
			:year year
			:month m
			:day 
			(last-day-of-gregorian-month m year)))
		      date))))
	 (day (- date
		 (1- (absolute-from-gregorian
		      (make-date :year year :month month))))))
    (make-date :year year :month month :day day)))

#+nil
(gregorian-from-absolute 60)
#+nil
(gregorian-from-absolute 
 (absolute-from-gregorian (make-date :year 2011 
				     :month 7 
				     :day 25)))

;; ISO week 1 is the one that includes the first Thursday of the year
;; this is equivalent to the week which includes 4. January

(defun kday-on-or-before (date k)
  "k=0 is sunday, k=1 monday"
  (declare (type integer date)
	   (type (integer 0 6) k))
  (- date (mod (- date k)
	       7)))

(defun kday-on-or-after (date k)
  (kday-on-or-before date (+ k 6)))

(defun kday-nearest (date k)
  (kday-on-or-before date (+ 3 k)))

(defun kday-previous (date k)
  (kday-on-or-before date (- k 1)))

(defun kday-following (date k)
  (kday-on-or-before date (+ 7 k)))

#+nil
(kday-on-or-before 60 2)

(defun absolute-from-iso (date)
  (assert (iso-p date))
  (destructuring-bind (year week day type) date
    (declare (ignore type))
    (+ (kday-on-or-before
	(absolute-from-gregorian (make-date :year year
					    :month 1
					    :day 4))
	1) ;; days prior in years
       (* 7 (1- week)) ;; days prior in weeks this year
       (1- day)) ;; prior days this week
    ))
#+nil
(absolute-from-iso (make-iso-date :week 3))

(defun iso-from-absolute (date)
  (declare (type integer date))
  (let* ((approx (extract-year 
		  (gregorian-from-absolute (- date 3))))
	 (year (+ approx 
		  (if (<= (absolute-from-iso 
			   (make-iso-date :year (1+ approx)))
			  date)
		      1 
		      0)))
	 (week (1+ (floor (- date (absolute-from-iso
				  (make-iso-date :year year)))
			 7)))
	 (day (if (= 0 (mod date 7))
		  7 ;; sunday is 7
		  (mod date 7))))
    (declare (type (integer 1 7) day)
	     (type (integer 1 53) week)
	     (type integer year))
    (make-iso-date :year year
		   :week week
		   :day day)))

#+nil
(iso-from-absolute 15)

#+nil
(gregorian-from-absolute
 (absolute-from-iso 
  (iso-from-absolute
   (absolute-from-gregorian
    (make-date :year 2011
	       :month 7
	       :day 25)))))

(defun d (y m d)
  (make-date :year y :month m :day d))

(defparameter *bank-holidays*
  (list (d 2008 1 1)
	(d 2008 3 21)
	(d 2008 3 24)
	(d 2008 5 5)
	(d 2008 5 26)
	(d 2008 8 25)
	(d 2008 12 25)
	(d 2008 12 26)
	
	(d 2009 1 1)
	(d 2009 4 10)
	(d 2009 4 13)
	(d 2009 5 4)
	(d 2009 5 25)
	(d 2009 8 31)
	(d 2009 12 25)
	(d 2009 12 28)
	
	(d 2010 1 1)
	(d 2010 4 2)
	(d 2010 4 5)
	(d 2010 5 3)
	(d 2010 5 31)
	(d 2010 8 30)
	(d 2010 12 27)
	(d 2010 12 28)
	
	(d 2011 1 3)
	(d 2011 4 22)
	(d 2011 4 25)
	(d 2011 5 2)
	(d 2011 5 30)
	(d 2011 8 29)
	(d 2011 12 26)
	(d 2011 12 27)))

(defparameter *vacation*
  (list (list (d 2008 12 22) (d 2009 1 9))
	(list (d 2009 2 6) (d 2009 2 16))
	(list (d 2009 5 5) (d 2009 5 8))
	(list (d 2009 8 17) (d 2009 8 20))
	(list (d 2009 11 11) (d 2009 11 13))
	(list (d 2009 6 8) (d 2009 6 9))
	(list (d 2009 12 22) (d 2010 1 20))
	(list (d 2010 4 2) (d 2010 4 6))
	(list (d 2010 5 26) (d 2010 5 28))
	(list (d 2010 12 21) (d 2011 1 8))
	(list (d 2011 2 17) (d 2011 2 22))
	(list (d 2011 4 26) (d 2011 4 28))
	(list (d 2011 5 3) (d 2011 5 5))))

(defun expand-vacation ()
 (let ((res nil))
   (dolist (r *vacation*)
     (destructuring-bind (start end) r 
       (setf res 
	     (append res (loop for i from (absolute-from-gregorian start)
			    upto (absolute-from-gregorian end) collect i)))))
   res))

(defun has-iso-53-weeks-p (year)
 (/= (1+ year) (first 
		(iso-from-absolute
		 (absolute-from-iso (make-iso-date :year year :week 53 :day 1))))))

(defun weeks-in-year (year)
  (if (has-iso-53-weeks-p year) 53 52))

#+nil
(loop for y in '(2004 2009 2015) collect
     (weeks-in-year y))

(defun absolute-iso-week (start date)
  (assert (iso-p date))
  (assert (iso-p start))
  (let ((res 0))
   (destructuring-bind (sy sw sd stype) start
     (destructuring-bind (y w d type) date
       (setf res (- w sw))
       (loop for j from sy below y do
	    (incf res (weeks-in-year j)))))
   res))

#+nil
(absolute-iso-week (make-iso-date :year 2008 :week 7 :day 3) 
		   (make-iso-date :year 2010 :week 7 :day 3))

;; here i tried to put my real holidays in, but i didn't take enough
;; and it makes everything really complicated
#+nil
(progn ; with-open-file 
       #+nil (*standard-output* "/dev/shm/o.txt" :direction :output
				:if-exists :supersede
				:if-does-not-exist :create)
 (let ((start (absolute-from-gregorian (make-date :year 2008 :month 7 :day 10)))
       (end (absolute-from-gregorian (make-date :year 2011 :month 7 :day 10)))
       (j 0)
       (bank-abs (mapcar #'absolute-from-gregorian *bank-holidays*))
       (vac-abs (expand-vacation))
       (weeks (make-array 157 :initial-element nil))
       (res nil))
   (loop for i from start upto end do
	(let ((iso (iso-from-absolute i)))
	  (destructuring-bind (y w d type) iso
	    (unless (or (= 7 d)
			(= 6 d)
			(member i bank-abs)
			(member i vac-abs))
	      (push i
		    (aref weeks (absolute-iso-week (iso-from-absolute start)
						   iso)))))))
   (loop for i below (length weeks) do
	(let* ((months (mapcar #'(lambda (x) (second (gregorian-from-absolute x)))
			       (reverse (aref weeks i))))
	       (need-split (and months
				(some #'(lambda (x) (/= (first months) x)) months))))
	  (flet ((q (ls)
		   (* (/ 10s0) (round (* 10 (* 7 30 (/ 31s0) (length ls))))))
		 (p (ls)
		   (when ls
		     (destructuring-bind (y m d ty) (gregorian-from-absolute (first ls))
		       (format nil "~d-~2,'0d-~2,'0d" y m d)))))
	    (if need-split
		(progn
		  (push (list (1+ i) 
			      (first months)
			      (p (aref weeks i))
			      (q (loop for e in months when (= e (first months)) collect e)))
			res)
		  (push
		   (list (1+ i) 
			 (1+ (first months))
			 (p (aref weeks i))
			 (q (loop for e in months when (= e (1+ (first months))) collect e)))
		   res))
		(push (list (1+ i) 
			    (first months)
			    (p (aref weeks i))
			    (q months))
		      res)))))
   (format t "~{~{ ~3d ~3d ~a ~2,1f ~}~%~}~%"(reverse res))
   (defparameter *q* res)))

(/ 660 682)
(/ (* 7 30) 31)


(* 660 7)

#+nil
(reduce #'+
	(mapcar #'second *q*))

;; its a 2.7hours more due to rounding

(defun years-bank-holidays (year)
 (remove-if #'(lambda (x) (/= year (extract-year x))) *bank-holidays*))


;; the goal is: distribute 220 working days into 3 years from 10 jul
;; 2008 on. 41 annual leave (containing 8 bank holidays) put the rest
;; in december..january because this is the only time with compulsary
;; holiday
;; now i have to create 33 more holidays

;; there are always two bank holidays at the end of december and one
;; on the start of january

;; 23 days in december
;; 13 days in january (in 2011 move 23 days there from december)

(defun simple-holiday-p (abs-date)
  (destructuring-bind (y w d type) (iso-from-absolute abs-date)
    (declare (ignore type))
    (or (<= (+ d (* w 7)) ;; holiday in january
	    (if (= y 2011) 
		13
		(+ 13 23)))
	(<= (- (absolute-from-gregorian ;; holiday in december
			       (make-date :year y  
					  :month 12
					  :day 31))
			      23) abs-date))))

(defun weekend-p (abs-date)
  (destructuring-bind (y w d type) (iso-from-absolute abs-date)
    (declare (ignore type y w))
    (or (= 7 d)
	(= 6 d))))

(defun martin-project-month (abs-date)
  (let* ((g (gregorian-from-absolute abs-date))
	 (y (extract-year g))
	 (m (extract-month g)))
    (+ m (* 12 (- y 2008)))))



(defun find-unique-weeks (wks)
 (let ((res nil)) 
   (loop for e in (mapcar (lambda (x) (destructuring-bind (w d) x
				   w)) wks)
      do
      (unless (member e res)
	(push e res)))
   (reverse res)))

(defun find-week-and-friday (w)
 (let* ((wu (find-unique-weeks w)))
   (loop for e in wu collect
	(destructuring-bind (w d) 
	    (first (remove-if (lambda (x) (destructuring-bind (w d) x
				       (/= w e))) w))
	  (let ((iso (iso-from-absolute d)))
	    (destructuring-bind (y w d ty) iso
	      (list w
		    (gregorian-from-absolute (absolute-from-iso (make-iso-date :year y
									       :week w
									       :day 5))))))))))
#+nil
(find-week-and-friday (aref *workr* 7))

(defun print-gregorian (date)
  (let ((y (extract-year date))
	(m (extract-month date))
	(d (extract-day date)))
    (format nil "~4d-~2,'0d-~2,'0d" y m d)))


(defun distribute-weeks-into-months (weeks)
  (macrolet ((appendf (place ls)
	       `(setf ,place (append ,place ,ls))))
    (let ((month-res (make-array (* 12 4) 
				 :initial-element nil)))
      (loop for w below (length weeks) do
	   (dolist (d (reverse (aref weeks w)))
	     (appendf (aref month-res (martin-project-month d))
		      (list (list w d)))))
      (let ((month-wk (make-array (* 12 4)
				  :initial-element nil)))
	(dotimes (m (length month-res))
	  (let* ((wks (aref month-res m))
		 (unique-wks (find-unique-weeks wks)))
	    (setf (aref month-wk m)
		  (loop for e in unique-wks collect
		       (length (remove-if #'(lambda (x) (destructuring-bind (w d) x
						     (/= w e))) wks))))))
	(values month-wk month-res)))))



(defparameter *tex-preamble*
  "\\documentclass[landscape]{scrartcl}
\\usepackage{pdfpages}
\\usepackage[absolute]{textpos} 

\\setlength{\\TPHorizModule}{1mm}
\\setlength{\\TPVertModule}{\\TPHorizModule}
\\textblockorigin{10mm}{10mm} % start everything near the top-left corner
\\setlength{\\parindent}{0pt}

\\begin{document}
")

(defparameter *tex-coda*
  "
\\includepdf{/home/martin/0725/cal/timesheet.pdf}
\\end{document}
")

#+nil
(let ((start (absolute-from-gregorian (make-date :year 2008 :month 7 :day 10)))
      (end (absolute-from-gregorian (make-date :year 2011 :month 7 :day 10)))
      (bank-abs (mapcar #'absolute-from-gregorian *bank-holidays*))
      (weeks (make-array 157 :initial-element nil))
      (holweeks (make-array 157 :initial-element nil)))
  ;; put all working days into weeks
  ;; and all holidays and bank holidays into holweeks 
  (loop for i from start upto end do
       (let ((iso (iso-from-absolute i)))
	 (unless (or (weekend-p i) ;; remove weekends, bankholidays and holidays
		     (member i bank-abs)
		     (simple-holiday-p i))
	   (push i
		 (aref weeks (absolute-iso-week (iso-from-absolute start)
						iso))))
	 (when (and (simple-holiday-p i)
		    (not (weekend-p i)))
	   (push i (aref holweeks (absolute-iso-week (iso-from-absolute start)
						     iso))))))
  ;; for each month list all contained weeks (partial weeks will appear in two months)
  ;; track week number (I guess they want the ISO week) and the gregorian ending day (friday)
  ;; work package ref
  ;; rtd and annual leave
  ;; total (rtd+annual leave)
  #+nil
  (loop for i below (length holweeks) do
       (format t "~a~%" (list (length (reverse (aref weeks i)))
			      (length (reverse (aref holweeks i))))))
  (multiple-value-bind (hol holr) (distribute-weeks-into-months holweeks)
    (multiple-value-bind (work workr)  (distribute-weeks-into-months weeks)
      (let* ((total-work (loop for e across work collect (* 7 (reduce #'+ e))))
	     (total-hol (loop for e across hol collect (* 7 (reduce #'+ e)))))
	(loop for m from 7 below (+ 7 (* 12 3)) do
	     (with-open-file (s (format nil "/dev/shm/o~3,'0d.tex" m)
				:direction :output
				:if-does-not-exist :create
				:if-exists :supersede)
	       (flet ((p (x y str &rest rest)
			(apply #'format s
			       (format nil "\\begin{textblock}{100}(~d,~d)~a\\end{textblock}~%"
				       x y str) rest)))
		(format s "~a" *tex-preamble*)
		(let* ((dayd (second (first (if (elt workr m)
						(elt workr m)
						(elt holr m)))))
		       (day (if dayd dayd (make-date :year (+ 2008 (floor m 12))
						     :month (if (= 0 (mod m 12))
								12
								(mod m 12))))))
		  (let ((g (gregorian-from-absolute day)))
		   (p 240 23 "~2,'0d/~4d ~d" 
		      (extract-month g)
		      (extract-year g)
		      day)
		   (defparameter *workr* workr)
		   ;; wk
		   (let ((w (get-weeks-of-month day)))
		     (loop for i below (length w) do
			  (p (+ 63 (* 33 i)) 68 "~d/~a" 
			     (elt w i)
			     (print-gregorian (gregorian-from-absolute
					       (absolute-from-iso
						(make-iso-date :year (extract-year g)
							       :week (elt w i)
							       :day 7)))))))))
		(defparameter *work* work)

		;; rtd
		(let ((weeks (elt work m)))
		  (loop for i below (length weeks) do
		       (p (+ 70 (* 32 i)) 82 "~a" (* 7 (elt weeks i)))))
		
		;; total rtd
		(p 230 82 "~2d" (elt total-work m))
		

		;; total leave
		(p 230 118 "~2d" (elt total-hol m))
		
		(let ((weeks (elt hol m)))
		  (when weeks
		    (loop for i below (length weeks) do
			 (p (+ 70 (* 32 i)) 118 "~a" (* 7 i)))))
		
		#+nil
		(loop for j from 10 below 200 by 10 do
		     (loop for i from 10 below 300 by 20 do
			  (p i j "~d,~d" i j)))
		(format s "~a" *tex-coda*))
	       ))
	(format t "----~%")
	(loop for i below (length hol) do
	     (format t "~d: ~a ~a~%" i (elt work i) (elt hol i)))
	(list hol work total-hol total-work
	      holr (loop for e across workr collect 
			(let ((q (first (last e))))
			  (when q
			    (destructuring-bind (w d) q
			      (list w d (gregorian-from-absolute d)))))))))))

(defun uniq (ls)
  (let ((old nil))
   (remove-if #'null
	      (loop for e in ls collect
		   (unless (eq old e) 
		     (setf old e))))))

(defun get-weeks-of-month (abs-day)
  (let* ((g (gregorian-from-absolute abs-day)))
    (destructuring-bind (y m d ty) g
      (let ((start (absolute-from-gregorian (make-date :year y :month m :day 1)))
	    (end (absolute-from-gregorian 
		  (make-date :year y :month m :day (last-day-of-gregorian-month m y)))))
	(loop for i from start upto end collect
	     (list (gregorian-from-absolute i) (extract-iso-week (iso-from-absolute i))))))))
#+nil
(get-weeks-of-month (absolute-from-gregorian (make-date :year 2009 :month 3 :day 1)))
#+nil
(get-weeks-of-month 733377 )