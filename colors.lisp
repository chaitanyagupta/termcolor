(cl:in-package #:cl-user)

(cl:defpackage #:colors
  (:use #:cl))

(in-package #:colors)

(defvar *fg-hash* (make-hash-table :test #'equal))
(defvar *bg-hash* (make-hash-table :test #'equal))

(defmacro defcolor (type name value)
  (let ((hash (ecase type
                (:fg '*fg-hash*)
                (:bg '*bg-hash*))))
    `(setf (gethash ,name ,hash) ,(format nil "~A" value))))

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

(export '+escape+)
(defconstant +escape+ (code-char #o33))

(export (list 'get-color 'get-fg 'get-bg))

(defun %get-color (fg bg bright)
  (format nil "~A[~A;~A~@[;~A~]m"
          +escape+
          (gethash (if bright :bright :dull) *fg-hash*)
          (gethash fg *fg-hash*)
          (and bg (gethash bg *bg-hash*))))

(defun get-color (fg &key bg bright)
  (%get-color fg bg bright))

(define-compiler-macro get-color (&whole form fg &key bg bright)
  (if (and (keywordp fg) (or (null bg) (keywordp bg)))
      (%get-color fg bg bright)
      form))
