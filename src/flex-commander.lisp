;;;; flex-commander.lisp

(in-package #:flex-commander)
(in-readtable :qtools)

(defun remove-last-char (s)
  (subseq s 0 (1- (length s))))

(defun remove-first-char (s)
  (subseq s 1))

(defun add-path-tail (path)
  (if (equal "/" (subseq path 0 (1- (length path))))
      path
      (concatenate 'string path "/")))

(defun get-directory-items (path)
  (let ((path (add-path-tail path)))
    (append
     (list "..")
     (mapcar
      #'(lambda (item) (remove-last-char (namestring  item)))
      (uiop/filesystem:subdirectories path))
     (mapcar
      #'(lambda (item) (remove-first-char (namestring  item)))
      (uiop/filesystem:directory-files path)))))

(defun first-slash? (str)
  "It's a directory. E.g.
  /home/user/.emacs - directory
  home/user/.Xresources - file"
  (equal (subseq str 0 1) "/"))

(defun format-directory-items (items)
  (mapcar
   #'(lambda (item)
       (if (first-slash? item)
           (concatenate 'string "/" (file-name item))
           (file-name item)))
   items))

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

(defun handle-change-location (&key path-widget panel-widget path-str)
  (q+:set-text path-widget path-str)
  (q+:clear panel-widget)
  (mapcar #'(lambda (x) (q+:add-item panel-widget x))
          (format-directory-items
           (get-directory-items path-str))))

(define-override (main-window key-press-event) (ev)
  ;; Assume :focus-widget is one of the panels
  (let* ((panel-widget (if (eq lst-left (q+:focus-widget *qapplication*))
                           lst-left
                           lst-right))
         (path-widget (if (eq lst-left (q+:focus-widget *qapplication*))
                          path-left
                          path-right))
         (enter-item (q+:text
                      (q+:current-item
                       (q+:focus-widget
                        *qapplication*))))
         (current-path (q+:text path-widget))
         (item-path (if (equal enter-item "..")
                        (uiop/pathname:parse-unix-namestring
                         (concatenate 'string current-path "/../"))
                        (uiop/pathname:merge-pathnames*
                         (concatenate 'string current-path "/")
                         enter-item)))
         (path-str (namestring item-path)))
    (cond
      ;; Signal return pressed.
      ((or (= (q+:key ev) (q+:qt.key_enter))
           (= (q+:key ev) (q+:qt.key_return)))
       (handle-change-location :path-widget path-widget
                               :panel-widget panel-widget
                               :path-str path-str))
      ;; F4 - editor
      ((= (q+:key ev) (q+:qt.key_f4))
       (uiop/run-program:run-program
        (concatenate
         'string
         "switch-to-emacsclient -n "
         "\"" path-str "\"")
        :ignore-error-status t)))))

;; (with-output-to-string (s)
;;   (uiop/run-program:run-program "ls" :output s))

(define-subwidget (main-window layout) (q+:make-qhboxlayout)
  (setf (q+:window-title main-window) "FlexCommander")
  (q+:set-font main-window (q+:make-Qfont "FiraMono" 12))
  (q+:set-spacing layout 3)
  (q+:set-contents-margins layout 3 3 3 3)
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
