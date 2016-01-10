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
               (:file "t/flex-commander"))
  :defsystem-depends-on (:qtools)
  :build-operation "qt-program-op"
  :build-pathname "flex-commander"
  :entry-point "flex-commander:main")

;; C-c C-k (ql:quickload :flex-commander)
;; (load #P"flex-commander.asd")
;; (asdf:operate :build-op :flex-commander)
