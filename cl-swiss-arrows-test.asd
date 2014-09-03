(asdf:defsystem #:cl-swiss-arrows-test
  :description "Tests for cl-swiss-arrows."
  :version "0.1"
  :licence "Public Domain"
  :depends-on (:cl-swiss-arrows :fiveam)
  :serial t
  :components ((:file "t/package")
               (:file "t/arrows")))

