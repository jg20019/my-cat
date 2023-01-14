(ql:quickload :my-cat/bin)

(sb-ext:save-lisp-and-die "my-cat" 
 :toplevel 'my-cat:main
 :executable t)
