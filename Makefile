LISP ?= sbcl

all: clean build

clean: 
	rm -f my-cat

build:
	$(LISP) \
		--eval '(ql:quickload :my-cat/bin)' \
		--eval '(asdf:make :my-cat/bin)'
