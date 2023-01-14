(in-package :my-cat)

(defmacro exit-on-ctrl-c (&body body) 
  `(handler-case (with-user-abort:with-user-abort (progn ,@body))
     (with-user-abort:user-abort () (sb-ext:exit :code 130))))

(defparameter *option-number-non-blank* 
  (adopt:make-option 'number-non-blank
    :short #\b
    :long "number-non-blank"
    :help "number non empty lines overrides -n"
    :reduce (constantly t)))

(defparameter *option-show-ends* 
  (adopt:make-option 'show-ends
    :short #\E
    :long "show-ends"
    :help "display $ at the end of each line"
    :reduce (constantly t)))

(defparameter *option-number* 
  (adopt:make-option 'number
   :short #\n
   :long "number"
   :help "number all output lines"
   :reduce (constantly t)))

(defparameter *option-squeeze-blank* 
  (adopt:make-option 'squeeze-blank
   :short #\s
   :long "squeeze-blank"
   :help "suppress repeated empty output lines"
   :reduce (constantly t)))

(defparameter *option-help*
  (adopt:make-option 'help
   :short #\h
   :long "help"
   :help "Display help and exit."
   :reduce (constantly t)))

(defparameter *ui* 
  (adopt:make-interface
    :name "my-cat"
    :summary "my-cat - concatenate files and print them to standard output"
    :usage "[OPTIONS]... [FILE]..."
    :help (format nil "Concatenate FILE(s) to standard output. ~
                       ~
                       With no FILE, or when FILE is -, read standard input.")
    :contents (list *option-number-non-blank* 
                    *option-show-ends*
                    *option-number*
                    *option-squeeze-blank*
                    *option-help*)))

(defun main ()
  (sb-ext:disable-debugger)
  (handler-case
    (multiple-value-bind (paths options) (adopt:parse-options *ui*) 
      (when (gethash 'help options) 
        (adopt:print-help-and-exit *ui*))
      (let* ((number-non-blank (gethash 'number-non-blank options)) 
             (number-all-lines (and (not number-non-blank)
                                    (gethash 'number options)))
             (show-ends (gethash 'show-ends options))
             (squeeze-blanks (gethash 'squeeze-blank options))
             (config (make-cat-config :number-all-lines number-all-lines
                                      :number-non-blank-lines number-non-blank
                                      :show-ends show-ends
                                      :squeeze-blanks squeeze-blanks)))
        (exit-on-ctrl-c (run paths config))))
    (error (c) 
     (adopt:print-error-and-exit c))))
