(cl:in-package #:cl-user)

(cl:defpackage #:termcolor
  (:use #:cl)
  (:export #:color))

(in-package #:termcolor)

(defvar *fg-hash* (make-hash-table :test #'equal))
(defvar *bg-hash* (make-hash-table :test #'equal))

(defun defcolor (type name value)
  (let ((hash (ecase type
                (:fg *fg-hash*)
                (:bg *bg-hash*))))
    (setf (gethash name hash) value)))

(defcolor :FG :DULL "0")
(defcolor :FG :BRIGHT "1")

(defcolor :FG :BLACK "30")
(defcolor :FG :RED "31")
(defcolor :FG :GREEN "32")
(defcolor :FG :YELLOW "33")
(defcolor :FG :BLUE "34")
(defcolor :FG :VIOLET "35")
(defcolor :FG :CYAN "36")
(defcolor :FG :WHITE "37")

(defcolor :FG :NULL "00")

(defcolor :BG :BLACK "40")
(defcolor :BG :RED "41")
(defcolor :BG :GREEN "42")
(defcolor :BG :YELLOW "43")
(defcolor :BG :BLUE "44")
(defcolor :BG :VIOLET "45")
(defcolor :BG :CYAN "46")
(defcolor :BG :WHITE "47")

(defcolor :BG :NULL "00")

(defconstant +escape+ (code-char #o33))

(defun %color (fg bg bright)
  (let ((fg-code (gethash fg *fg-hash*))
        (bg-code (gethash bg *bg-hash*)))
    (assert fg-code () "Foreground color invalid: ~A" fg)
    (when bg (assert bg-code () "Background color invalid: ~A" bg))
    (format nil "~A[~A;~A~@[;~A~]m"
            +escape+
            (gethash (if bright :bright :dull) *fg-hash*)
            fg-code
            bg-code)))

(defun color (fg &key bg bright)
  (%color fg bg bright))

(define-compiler-macro color (&whole form fg &key bg bright)
  (if (and (keywordp fg) (or (null bg) (keywordp bg)))
      (%color fg bg bright)
      form))
