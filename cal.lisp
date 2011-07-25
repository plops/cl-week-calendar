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