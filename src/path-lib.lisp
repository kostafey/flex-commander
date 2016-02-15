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

(defun get-directory-items (path &optional init-list)
  (let ((path (add-path-tail path))
        (init-list (if init-list init-list (list))))
    (append
     init-list
     (mapcar
      #'(lambda (item) (remove-last-char (namestring  item)))
      (uiop/filesystem:subdirectories path))
     (mapcar
      #'(lambda (item) (remove-first-char (namestring  item)))
      (uiop/filesystem:directory-files path)))))

(defun first-slash? (str)
  "It's a directory. E.g.
  /home/user/.emacs.d - directory
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
  (let ((path (if (first-slash? path)
                  (remove-first-char path)
                  path))
        (len-t (length typed-text))
        (len-p (length path)))
    (and (>= len-p len-t)
         (equal (subseq path 0 len-t) typed-text))))

(defun find-matched-path-list (path-list typed-text)
  (remove-if-not (lambda (x) (matches? x typed-text))
                 path-list))

;; (find-matched-path-list (get-directory-items "/") "b")
