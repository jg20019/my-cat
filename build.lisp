(ql:quickload :my-cat/bin)

(sb-ext:save-lisp-and-die #p"my-cat" 
 :toplevel 'my-cat:main
 :executable t)
