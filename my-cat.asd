(defsystem :my-cat
  :serial t
  :depends-on (:with-user-abort)
  :components ((:file "packages")
               (:file "my-cat")))

(defsystem :my-cat/bin
  :depends-on (:adopt :my-cat)
  :components ((:file "main")))
