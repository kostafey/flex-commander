;;;; flex-commander.lisp

(in-package #:flex-commander)
(in-readtable :qtools)

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

;;------------------------------------------------------------
;; left

(define-subwidget (main-window lst-left)
                  (q+:make-qlistwidget main-window)
  (mapcar #'(lambda (x) (q+:add-item lst-left x))
          (get-directory-items "/")))

(define-subwidget (main-window path-left)
                  (q+:make-qlineedit "/"))

;;------------------------------------------------------------
;; right

(define-subwidget (main-window lst-right)
                  (q+:make-qlistwidget main-window)
  (mapcar #'(lambda (x) (q+:add-item lst-right x))
          (get-directory-items "/")))

(define-subwidget (main-window path-right)
                  (q+:make-qlineedit "/"))

;;
;;------------------------------------------------------------

(defun info (parent msg)
  (q+:qmessagebox-information parent "Info" msg))

(defun handle-change-location (&key path-widget panel-widget enter-item)
  (let* ((current-path (q+:text path-widget))
         (path (if (equal (subseq enter-item 0 1) "/")
                   (uiop/pathname:merge-pathnames*
                    (concatenate 'string current-path "/")
                    enter-item)
                   (if (equal enter-item "..")
                       (uiop/pathname:parse-unix-namestring
                        (concatenate 'string current-path "/../")))))
         (path-str (namestring path)))
    (q+:set-text path-widget path-str)
    (q+:clear panel-widget)
    (mapcar #'(lambda (x) (q+:add-item panel-widget x))
            (get-directory-items path-str))))

(define-override (main-window key-press-event) (ev)
  (cond ;; Signal return pressed.
    ((or (= (q+:key ev) (q+:qt.key_enter))
         (= (q+:key ev) (q+:qt.key_return)))
     (let ((panel (if (eq lst-left (q+:focus-widget *qapplication*))
                      lst-left
                      lst-right))
           (path (if (eq lst-left (q+:focus-widget *qapplication*))
                     path-left
                     path-right)))
       (handle-change-location :path-widget path
                               :panel-widget panel
                               :enter-item (q+:text
                                            (q+:current-item
                                             (q+:focus-widget
                                              *qapplication*))))))))

(define-subwidget (main-window layout) (q+:make-qhboxlayout)
  (setf (q+:window-title main-window) "FlexCommander")
  (let ((ll (q+:make-qvboxlayout)))
    (q+:add-widget ll path-left)
    (q+:add-widget ll lst-left)
    (q+:add-layout layout ll))
  (let ((rl (q+:make-qvboxlayout)))
    (q+:add-widget rl path-right)
    (q+:add-widget rl lst-right)
    (q+:add-layout layout rl))
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
