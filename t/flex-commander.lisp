;;;; flex-commander.lisp

(defpackage t.flex-commander
  (:use #:cl
        #:prove
        #:flex-commander))

(in-package flex-commander)

(plan 2)

(is '("123" "12345" "12")
    (find-matched-path-list
     '("123" "12345" "1345" "12" "21") "12")
    :test #'equalp)

(ok (consp (get-directory-items "/")))

(finalize)

;; (prove:run :flex-commander)
