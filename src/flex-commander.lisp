;;;; flex-commander.lisp

(in-package #:flex-commander)
(in-readtable :qtools)

(define-widget main-window (QMainWindow)
  ())

;;------------------------------------------------------------
;; left

(define-subwidget (main-window path-left)
                  (q+:make-qlineedit "/"))

(define-subwidget (main-window lst-left)
                  (q+:make-qlistwidget main-window)
  (draw-directory-items lst-left "/"))

;;------------------------------------------------------------
;; right

(define-subwidget (main-window lst-right)
                  (q+:make-qlistwidget main-window)
  (draw-directory-items lst-right "/"))

(define-subwidget (main-window path-right)
                  (q+:make-qlineedit "/"))

;;
;;------------------------------------------------------------

(defun info (parent msg)
  (q+:qmessagebox-information parent "Info" msg))

(defun draw-directory-items (panel-widget path-str)
  (if (> (q+:count panel-widget) 0)
      (dolist
          (i (alexandria:iota (- (q+:count panel-widget) 1)
                              :start (- (q+:count panel-widget) 1)
                              :step -1))
        (q+:remove-item-widget
         panel-widget
         (q+:take-item panel-widget i)))
      (q+:add-item panel-widget ".."))
  (mapcar #'(lambda (x)
              (let ((item (q+:make-qlistwidgetitem)))
                (if (first-slash? x)
                    (progn
                      (q+:set-text item (remove-first-char x))
                      (q+:set-icon item (q+:make-qicon
                                         "../resources/folder.png"))
                      (q+:set-whats-this item "folder"))
                    (progn
                      (q+:set-text item x)
                      (q+:set-icon item (q+:make-qicon
                                         "../resources/file.png"))))
                (q+:add-item panel-widget item)))
          (format-directory-items
           (get-directory-items path-str))))

(defun handle-change-location (&key path-widget panel-widget path-str)
  (q+:set-text path-widget path-str)
  (draw-directory-items panel-widget path-str))

(define-override (main-window key-press-event) (ev)
  ;; Assume :focus-widget is one of the panels
  (if (or (eq lst-left (q+:focus-widget *qapplication*))
          (eq lst-right (q+:focus-widget *qapplication*)))
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
                            (if (not (equal current-path "/"))
                             (uiop/pathname:parse-unix-namestring
                              (concatenate 'string current-path "/../"))
                             "/")
                            (uiop/pathname:merge-pathnames*
                             (concatenate 'string current-path "/")
                             enter-item)))
             (path-str (namestring item-path))
             (is-folder? (or (equal enter-item "..")
                             (equal (q+:whats-this (q+:current-item
                                                    (q+:focus-widget
                                                     *qapplication*)))
                                    "folder"))))
        (cond
          ;; Signal return pressed.
          ((and is-folder?
                (or (= (q+:key ev) (q+:qt.key_enter))
                    (= (q+:key ev) (q+:qt.key_return))))
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
            :ignore-error-status t))))))

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
