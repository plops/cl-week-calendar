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
  (first date))

(defun extract-month (date)
  (second date))

(defun extract-day (date)
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

(absolute-iso-week (make-iso-date :year 2008 :week 7 :day 3) 
		   (make-iso-date :year 2010 :week 7 :day 3))

(let ((start (absolute-from-gregorian (make-date :year 2008 :month 7 :day 10)))
      (end (absolute-from-gregorian (make-date :year 2011 :month 7 :day 10)))
      (j 0)
      (bank-abs (mapcar #'absolute-from-gregorian *bank-holidays*))
      (vac-abs (expand-vacation))
      (weeks (make-array 157 :initial-element nil)))
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
	 (if need-split
	     (format t "~a~%~a~%" 
		     (list (1+ i) 
			   (find (first months) months))
		     (list (1+ i) 
			   (find (1+ (first months)) months)))
	     (format t "~a~%" (list (1+ i) 
				 months))))))

(/ 660 682)
(/ (* 7 30) 31)