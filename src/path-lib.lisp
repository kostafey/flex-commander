(in-package #:flex-commander)
(in-readtable :qtools)

(defun first-slash? (str)
  "It's a directory. E.g.
  /home/user/.emacs.d - directory
  home/user/.Xresources - file"
  (equal (subseq str 0 1) "/"))

(defun last-slash? (str)
  (equal (subseq str (1- (length str)) (length str)) "/"))

(defun remove-last-char (s)
  (subseq s 0 (1- (length s))))

(defun remove-first-char (s)
  (subseq s 1))

(defun add-path-tail (path)
  (if (last-slash? path)
      path
      (concatenate 'string path "/")))

(defun get-directory-items (path)
  (let* ((path (add-path-tail path))
         (path-pattern (concatenate 'string path "*.*")))
    (uiop/filesystem:directory* (pathname path-pattern))))

(defun filter-list-into-two-parts (predicate list)
  (loop for x in list
        if (funcall predicate x) collect x into yes
          else collect x into no
        finally (return (values yes no))))

(defun format-directory-items (items)
  (multiple-value-bind (dirs files)
      (filter-list-into-two-parts
       'directory-p
       items)
    (append
     (mapcar
      #'(lambda (item)
          (concatenate 'string "/" (directory-name (namestring item))))
      dirs)
     (mapcar
      #'(lambda (item)
          (file-name (namestring item)))
      files))))

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
