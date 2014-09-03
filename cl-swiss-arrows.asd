(asdf:defsystem #:cl-swiss-arrows
  :description "Port of Clojure swiss-arrows."
  :version "0.1"
  :licence "Public Domain"
  :serial t
  :components ((:file "src/package")
               (:file "src/arrows"))
  :in-order-to ((asdf:test-op (asdf:load-op cl-swiss-arrows-test))))

