;;;; flex-commander.asd

(asdf:defsystem #:flex-commander
  :description "Describe flex-commander here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (:swank
               :prove
               :qtools :qtcore :qtgui)
  :components ((:file "package")
               (:file "src/flex-commander")
               (:file "t/flex-commander")))

;; C-c C-k (ql:quickload :flex-commander)

