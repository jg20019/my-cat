(defsystem :my-cat
  :serial t
  :components ((:file "packages")
               (:file "my-cat")))

(defsystem :my-cat/bin
  :depends-on (:with-user-abort :adopt :my-cat)
  :components ((:file "main")))
