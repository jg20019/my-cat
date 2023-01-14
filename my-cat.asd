(defsystem :my-cat
  :serial t
  :components ((:file "packages")
               (:file "my-cat")))

(defsystem :my-cat/bin
  :depends-on (:with-user-abort :adopt :my-cat)
  :build-operation program-op
  :build-pathname "my-cat"
  :entry-point "my-cat:main"
  :components ((:file "main")))
