;;;; flex-commander.asd

(asdf:defsystem #:flex-commander
  :description "Describe flex-commander here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (:swank
               :prove
               :pathname-utils
               :alexandria
               :qtools :qtcore :qtgui)
  :components ((:file "package")
               (:file "src/flex-commander")
               (:file "src/path-lib")
               (:file "t/flex-commander")
               (:static-file "resources/file.png")
               (:static-file "resources/folder.png"))
  :defsystem-depends-on (:qtools)
  :build-operation "qt-program-op"
  :build-pathname "flex-commander"
  :entry-point "flex-commander:main")

;; C-c C-k (ql:quickload :flex-commander)
;; (load #P"flex-commander.asd")
;; (asdf:operate :build-op :flex-commander)

;; Windows
;; cd %HOMEPATH%/.roswell/lisp/quicklisp/local-projects/flex-commander/bin/
;; ros run -core
;; (ql:quickload :flex-commander)
;; (asdf:operate :build-op :flex-commander)
