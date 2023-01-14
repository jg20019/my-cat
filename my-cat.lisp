;;;; my-cat.lisp

(in-package :my-cat)

;;; Configuration for cat program
(defstruct cat-config number-all-lines 
                      number-non-blank-lines
                      show-ends
                      squeeze-blanks)

(defparameter *line-number* 1)

(defun blank-p (line) 
  "Is this line blank?"
  (zerop (length line)))

(defun number-line-p (line number-all-lines number-non-blank-lines) 
  "Should I print a number?"

  ;; number-non-blank-lines should override number-all-lines
  ;; so even if number-all-line is true, we check that number-non-blank-lines
  ;; is false.
  (or (and number-all-lines (not number-non-blank-lines))
      (and number-non-blank-lines (not (blank-p line)))))

(defun skip-line-p (line last-line squeeze-blanks) 
  "Should I skip printing this line?"
  (and squeeze-blanks
       (blank-p last-line)
       (blank-p line)))

(defun cat (line last-line config) 
  "Output line according to config"
  (let ((number-all-lines (cat-config-number-all-lines config))
        (number-non-blank-lines (cat-config-number-non-blank-lines config))
        (squeeze-blanks (cat-config-squeeze-blanks config))
        (show-ends (cat-config-show-ends config)))

    (when (skip-line-p line last-line squeeze-blanks) 
      (return-from cat))

    (when (number-line-p line number-all-lines number-non-blank-lines)
      (format t "    ~d  " *line-number*)
      (incf *line-number*))

    (if show-ends
        (format t "~a$~%" line)
        (write-line line))))

(defun process-file (fn stream)
  "Call fn on each line in given stream."
  (loop for line = (read-line stream nil) 
        and last-line = " " then line
        while line
        do (funcall fn line last-line)))

(defun run (fn paths &optional (*line-number* 1))
  "Run fn on all of given paths."
  (dolist (path paths) 
    (if (string= "-" path) 
        (process-file fn *standard-input*)
        (with-open-file (stream path) 
          (process-file fn stream)))))
