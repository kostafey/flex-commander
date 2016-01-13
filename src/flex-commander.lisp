;;;; flex-commander.lisp

(in-package #:flex-commander)
(in-readtable :qtools)

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

(define-widget main-window (QMainWindow)
  ())

(define-subwidget (main-window lst-left)
                  (q+:make-qlistwidget main-window)
  (mapcar #'(lambda (x) (q+:add-item lst-left x))
          (get-directory-items "/")))

(define-subwidget (main-window lst-right)
                  (q+:make-qlistwidget main-window)
  (mapcar #'(lambda (x) (q+:add-item lst-right x))
          (get-directory-items "/")))

(define-subwidget (main-window layout) (q+:make-qhboxlayout)
  (setf (q+:window-title main-window) "FlexCommander")
  (q+:add-widget layout lst-left)
  (q+:add-widget layout lst-right)
  (let ((central-widget (q+:make-qwidget main-window)))
    (setf (q+:layout central-widget) layout)
    (setf (q+:central-widget main-window) central-widget)))

(defun open-file ())
(defun save-file (&optional file-name))

(define-menu (main-window File)
  (:item ("Open..." (ctrl o))
    (open-file))
  (:separator)
  (:item ("Save" (ctrl s))
    (save-file))
  (:item ("Save As..." (ctrl alt s))
    (save-file NIL))
  (:menu "Export"
    (:item "PNG" (save-file NIL "png")))
  (:separator)
  (:item ("Quit" (ctrl q))
         (#_close main-window)))

(defun main ()
  (with-main-window (window (make-instance 'main-window))))

;; (main)
