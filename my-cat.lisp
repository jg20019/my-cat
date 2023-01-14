;;;; my-cat.lisp

(in-package :my-cat)

(defparameter *line-number* 1)
(defparameter *number-all-output-lines* nil)
(defparameter *number-non-blank-lines* nil)
(defparameter *show-ends* nil)
(defparameter *squeeze-blank* nil)
(defparameter *last-line* "   ") ;; the first line is always considered non-empty

(defun blank-p (line) 
  "Is this line blank?"
  (zerop (length line)))

(defun number-line-p (line) 
  (or *number-all-output-lines* 
      (and *number-non-blank-lines* (not (blank-p line)))))

(defun skip-line-p (line) 
  "Should I skip printing this line?"
  (and *squeeze-blank*
       (blank-p *last-line*)
       (blank-p line)))

(defun output-line (line) 
  (when (skip-line-p line) 
    (setf *last-line* line)
    (return-from output-line))

  (setf *last-line* line)

  (when (number-line-p line)
    (format t "    ~d  " *line-number*)
    (incf *line-number*))

  (if *show-ends*
      (format t "~a$~%" line)
      (write-line line)))

(defun cat (stream)
  (loop for line = (read-line stream nil) 
        while line
        do (output-line line)))

(defun run (paths)
  (let ((paths (if (null paths) (list "-") paths)))
    (setf *line-number* 1)
    (dolist (path paths) 
      (if (string= "-" path) 
          (cat *standard-input*)
          (with-open-file (stream path)
            (cat stream))))))
