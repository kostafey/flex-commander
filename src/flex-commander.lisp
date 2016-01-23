;;;; flex-commander.lisp

(in-package #:flex-commander)
(in-readtable :qtools)

(defparameter left-path "/")

(defun remove-last-char (s)
  (subseq s 0 (1- (length s))))

(defun remove-first-char (s)
  (subseq s 1))

(defun get-directory-items (path)
  (append
   (list "..")
   (mapcar
    #'(lambda (item) (remove-last-char (namestring  item)))
    (uiop/filesystem:subdirectories path))
   (mapcar
    #'(lambda (item) (remove-first-char (namestring  item)))
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

(defun info (parent msg)
  (q+:qmessagebox-information parent "Info" msg))

(defun handle-change-location (&key panel folder)
  (q+:clear panel)
  (mapcar #'(lambda (x) (q+:add-item panel x))
          (get-directory-items (concatenate 'string folder "/"))))

(define-override (main-window key-press-event) (ev)
  (cond ;; Signal return pressed.
    ((or (= (q+:key ev) (q+:qt.key_enter))
         (= (q+:key ev) (q+:qt.key_return)))
     (handle-change-location :panel (q+:focus-widget *qapplication*)
                             :folder (q+:text
                                      (q+:current-item
                                       (q+:focus-widget *qapplication*)))))))

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
