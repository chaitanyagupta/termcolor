(cl:in-package #:cl-user)

(cl:defpackage #:termcolor
  (:use #:cl)
  (:export #:*colorize* #:color #:fg #:bg #:style #:reset #:raw #:with-color))

(in-package #:termcolor)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *hashes*
    (list :fg (make-hash-table)
          :bg (make-hash-table)
          :style (make-hash-table)))

  (defun defcolor (type name value)
    (let ((hash (or (getf *hashes* type) (error "Invalid color type: ~A" type))))
      (setf (gethash name hash) value)))

  (defcolor :STYLE :BRIGHT       1)
  (defcolor :STYLE :DIM          2)
  (defcolor :STYLE :UNDERLINE    4)
  (defcolor :STYLE :NEGATIVE     7)
  (defcolor :STYLE :CONCEAL      8)
  (defcolor :STYLE :CROSS-OUT    9)
  (defcolor :STYLE :NORMAL       22)
  (defcolor :STYLE :NO-UNDERLINE 24)
  (defcolor :STYLE :POSITIVE     27)
  (defcolor :STYLE :REVEAL       28)
  (defcolor :STYLE :NO-CROSS-OUT 29)

  (defcolor :FG    :BLACK        30)
  (defcolor :FG    :RED          31)
  (defcolor :FG    :GREEN        32)
  (defcolor :FG    :YELLOW       33)
  (defcolor :FG    :BLUE         34)
  (defcolor :FG    :VIOLET       35)
  (defcolor :FG    :CYAN         36)
  (defcolor :FG    :WHITE        37)
  (defcolor :FG    :RESET        39)

  (defcolor :BG    :BLACK        40)
  (defcolor :BG    :RED          41)
  (defcolor :BG    :GREEN        42)
  (defcolor :BG    :YELLOW       43)
  (defcolor :BG    :BLUE         44)
  (defcolor :BG    :VIOLET       45)
  (defcolor :BG    :CYAN         46)
  (defcolor :BG    :WHITE        47)
  (defcolor :BG    :RESET        49)

  (defconstant +escape+ #\Esc)

  (defun get-color (type name)
    (or (gethash name (getf *hashes* type))
        (when name (error "Invalid color ~A: ~A" type name))))

  (defun %%color (&key fg bg style number)
    (assert (or fg bg style number) (fg bg style number)
            "%COLOR: Atleast one of the keyword args is required.")
    (format nil "~A[~{~@[~A~^;~]~}m"
            +escape+
            (remove-if #'null (list (get-color :style style)
                                    (get-color :fg fg)
                                    (get-color :bg bg)
                                    number)))))

(defun %color (&key fg bg style number)
  (%%color :fg fg :bg bg :style style :number number))

(define-compiler-macro %color (&whole form &key fg bg style number)
  (if (and (or (null fg) (keywordp fg))
           (or (null bg) (keywordp bg))
           (or (null style) (keywordp style))
           (or (null number) (integerp number)))
      (%%color :fg fg :bg bg :style style :number number)
      form))

(defvar *colorize* t)

(defun colorizep (stream)
  (or (eq *colorize* :force)
      (and *colorize* (interactive-stream-p (find-stream stream)))))

(defun write-color-string (string stream print)
  (cond ((null print) string)
        ((not (colorizep stream)) (values))
        (t (write-string string stream) (values))))

(defun find-stream (stream)
  (cond ((null stream) *standard-output*)
        ((eq stream t) *terminal-io*)
        (t stream)))

(defun color (&key fg bg style stream (print t))
  (write-color-string (%color :fg fg :bg bg :style style) stream print))

(defmacro def-colorfn (type)
  (let ((type-keyword (intern (string type) :keyword)))
    `(defun ,type (name &key stream (print t))
       (write-color-string (%color ,type-keyword name) stream print))))

(def-colorfn fg)
(def-colorfn bg)
(def-colorfn style)

(defun reset (&key stream (print t))
  (write-color-string (%color :number 0) stream print))

(defun raw (number &key stream (print t))
  (write-color-string (%color :number number) stream print))

(defmacro with-color ((&key fg bg style stream (colorize '*colorize*)) &body body)
  `(let ((*colorize* ,colorize))
     (unwind-protect
          (progn
            (color :fg ,fg :bg ,bg :style ,style :stream ,stream)
            ,@body)
       (reset :stream ,stream))))
