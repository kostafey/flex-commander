;;;; flex-commander.lisp

(defpackage t.flex-commander
  (:use #:cl+qt
        #:prove
        #:flex-commander)
  (:shadow :finalize))

(in-package t.flex-commander)

(plan 2)

(is '("123" "12345" "12")
    (flex-commander::find-matched-path-list
     '("123" "12345" "1345" "12" "21") "12")
    :test #'equalp)

(ok (consp (flex-commander::get-directory-items "/")))

(prove::finalize)

;; (prove:run :flex-commander)
