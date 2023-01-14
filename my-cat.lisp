;;;; my-cat.lisp

(in-package :my-cat)

;;; Configuration for cat program
(defstruct cat-config number-all-lines 
                      number-non-blank-lines
                      show-ends
                      squeeze-blanks)

(defparameter *line-number* 1)
(defparameter *last-line* "   ") ;; the first line is always considered non-empty

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

(defun skip-line-p (line squeeze-blanks) 
  "Should I skip printing this line?"
  (and squeeze-blanks
       (blank-p *last-line*)
       (blank-p line)))

(defun output-line (line config) 
  "Output line according to config"
  (let ((number-all-lines (cat-config-number-all-lines config))
        (number-non-blank-lines (cat-config-number-non-blank-lines config))
        (squeeze-blanks (cat-config-squeeze-blanks config))
        (show-ends (cat-config-show-ends config)))

    (when (skip-line-p line squeeze-blanks) 
      (setf *last-line* line)
      (return-from output-line))
    (setf *last-line* line)

    (when (number-line-p line number-all-lines number-non-blank-lines)
      (format t "    ~d  " *line-number*)
      (incf *line-number*))

    (if show-ends
        (format t "~a$~%" line)
        (write-line line))))

(defun cat (stream config)
  "Read lines from stream printing according to config"
  (loop for line = (read-line stream nil) 
        while line
        do (output-line line config)))

(defun run (paths config)
  (let ((paths (if (null paths) (list "-") paths)))
    (setf *line-number* 1)
    (dolist (path paths) 
      (if (string= "-" path) 
          (cat *standard-input* config)
          (with-open-file (stream path)
            (cat stream config))))))
