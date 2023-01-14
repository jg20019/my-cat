(ql:quickload :my-cat/bin)

(sb-ext:save-lisp-and-die #p"~/.lisp-bin/my-cat" 
 :toplevel 'my-cat:main
 :executable t)
