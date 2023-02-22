;;    November 2022
;; Su Mo Tu We Th Fr Sa
;;        1  2  3  4  5
;;  6  7  8  9 10 11 12
;; 13 14 15 16 17 18 19
;; 20 21 22 23 24 25 26
;; 27 28 29 30

(load "~/quicklisp/setup.lisp")

(load "~/quicklisp/setup.lisp")
(ql:quickload "local-time")
(ql:quickload :spinneret)

(defpackage calendar
  (:use :cl))

(in-package calendar)

(defun now ()
  (local-time:now))

(defun start-of-month (tm)
  "Revert the time to the first day of the month."
  (local-time:timestamp-minimize-part tm :day))

(defun last-of-month (tm)
  "Revert the time to the first day of the month."
  (local-time:timestamp-maximize-part tm :day))

(defun print-month (d)
  (print (concatenate 'string "  " (local-time:timestamp-month d) " " (local-time:timestamp-year d) "  "))
  (print "Su Mo Tu We Th Fr Sa"))

(defun start-of-week (d)
  "Is the provided day the start of a week?"
  (= 0 (local-time:timestamp-day-of-week d)))

(defun day-of-month (d)
  "Get the day of the month off a timestamp"
  (local-time:timestamp-day d))

(defun month-header ()
  "A header with all of the months."
  (spinneret::with-html
    (:tr (:th "Su") (:th "Mo") (:th "Tu") (:th "We") (:th "Th") (:th "Fr") (:th "Sa"))))

(defun num-month-days (d)
  "Get the number of days in the current month of the day"
  (local-time:days-in-month (local-time:timestamp-month d) (local-time:timestamp-year d)))

(defun dates-in-month (date)
  "Get all of the dates from the start to the end of the month that some date occupies."
  (let ((cur-date (start-of-month date)))
    (loop for i below (num-month-days date)
          collect (local-time:adjust-timestamp cur-date (offset :day i)))))

(defun split-by-week (days)
  "Split up a list of days by week, producing a list of list of day,
   where each day represents a week."
  (let ((weeks '())
        (cur-week '()))
    (loop for day in days
          do (progn
               (when (start-of-week day)
                 (setq weeks (cons (reverse cur-week) weeks))
                 (setq cur-week nil))
               (setq cur-week (cons day cur-week))))
    (reverse (cons (reverse cur-week) weeks))))

(defun pad-tds (week-dates)
  "Pad a week at the start or the end. Assume list is non-empty."
  (let ((to-cons
          (loop for i below (- 7 (length week-dates))
                collect :no-day))
        (to-end (start-of-week (car week-dates))))
    (if to-end
        (concatenate 'list week-dates to-cons)
        (concatenate 'list to-cons week-dates))))

(defun prefix-space (num)
  "Prefix a space to a number if only of length 1."
  (spinneret::with-html
    (let ((num-str (write-to-string num)))
      (if (equal (length num-str) 1)
          (:raw (concatenate 'string "&nbsp;" num-str))
          num-str))))

(defun weeks->table (weeks)
  "Convert weeks to a table."
  (spinneret::with-html
    (loop for week in weeks
          collect (:tr
                   (loop for day in (pad-tds week)
                         collect
                         (let ((is-day (not (equal day :no-day))))
                           (:td :class (when is-day "calendar-day")
                                (when is-day
                                  (let ((dindex (day-of-month day)))
                                    (if (= dindex (day-of-month (now)))
                                        (:b :class "current-day" dindex) dindex))))))))))

(defun year-string (d)
  (write-to-string (local-time:timestamp-year d)))

(defun month-string (d)
  (let ((m (local-time:timestamp-month d)))
    (nth (1- m) (list "January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"))))

(defun month-calendar-title (d)
  "Get the calendar title."
  (concatenate 'string (month-string d) " " (year-string d)))

(defun month-html (d)
  "Generate a month html calendar."
  (spinneret::with-html
    (:table :class "month-calendar"
            (month-header)
            (weeks->table (split-by-week (dates-in-month d))))))
