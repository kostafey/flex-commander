;;;; flex-commander.lisp

(in-package #:flex-commander)

;;; "flex-commander" goes here. Hacks and glory await!

(defun get-directory-items (path)
  (map
   'list
   'namestring
   (append (uiop/filesystem:subdirectories path)
           (uiop/filesystem:directory-files path))))

(defun matches? (path typed-text)
  (let ((len (length typed-text)))
    (equal (subseq path 0 len) typed-text)))

(defun find-matched-path-list (path-list typed-text)
  (remove-if-not (lambda (x) (matches? x typed-text))
                 path-list))
