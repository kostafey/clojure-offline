;;; clojure-offline.el -- The set of functions to help use clojure offline.

;;; Copyright © 2013 - Kostafey <kostafey@gmail.com>

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software Foundation,
;;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.  */

;;; Commentary

;; `clojure-offline-create-script' - Construct download and/or installation
;; script for certain clojure dependencies.
;;
;; See README.md for detail description.

(defvar clojure-offline-script-buffer-name "*clojure-offline*")

;;;###autoload
(defun clojure-offline-trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string
   "\\`[ \t\n]*" ""
   (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

;;;###autoload
(defun clojure-offline-concat-path (&rest folders)
  "Concatenate list of folders to the path.
E.g.
 (clojure-offline-concat-path (getenv \"HOME\") \".m2\" \"repository\")"
  (let ((path))
    (dolist (folder folders)
      (setq path (file-name-as-directory (concat path folder))))
    path))

;;----------------------------------------------------------------------
;;
;;;###autoload
(defun clojure-offline-parse-artifact (artifact-name)
  "Parse `artifact-name' to list (`group-id' `artifact-id' `version')
Input formats, e.g.:
 [org.clojure/clojure \"1.5.1\"]
 \"[org.clojure/clojure \"1.5.1\"]\"
 org.clojure:clojure:pom:1.5.1
Ouptut format, e.g.:
 (\"org.clojure\" \"clojure\" \"1.5.1\")"
  ;; lein format as string e.g. "[org.clojure/clojure \"1.5.1\"]"
  (let* ((artifact-name
          (if (and (not (vectorp artifact-name))
                   (stringp artifact-name)
                   (equal (substring (clojure-offline-trim-string artifact-name) 0 1) "["))
              (read (clojure-offline-trim-string artifact-name))
            artifact-name)))
    (let* ((is-lein-format (vectorp artifact-name))
           artifact-list
           group-and-artifact
           group-id
           artifact-id
           version)
      (if is-lein-format
          ;; lein format, e.g. [org.clojure/clojure "1.5.1"]
          (progn
            (setq group-and-artifact
                  (split-string (force-symbol-name (elt artifact-name 0))
                                "/"))
            (setq group-id (nth 0 group-and-artifact))
            (setq artifact-id (if (> (length group-and-artifact) 1)
                                  (nth 1 group-and-artifact)
                                (nth 0 group-and-artifact)))
            (setq version (elt artifact-name 1)))
        ;; maven format, e.g. "org.clojure:clojure:pom:1.5.1"
        (progn
          (setq artifact-list (split-string artifact-name ":"))
          (setq group-id (nth 0 artifact-list))
          (setq artifact-id (nth 1 artifact-list))
          (setq version (nth 3 artifact-list))))
      (list group-id artifact-id version))))

;;;###autoload
(defmacro clojure-offline-with-artifact (artifact-name &rest body)
  "Inject `group-id' `artifact-id' `version' local variables to the `body'
scope."
  `(let* ((artifact (clojure-offline-parse-artifact ,artifact-name))
          (group-id (nth 0 artifact))
          (artifact-id (nth 1 artifact))
          (version (nth 2 artifact)))
     ,@body))

;;----------------------------------------------------------------------
;;
;;;###autoload
(defun clojure-offline-form-urls (art-path)
  (list
   (concat "https://clojars.org/repo/" art-path ".jar --no-check-certificate")
   (concat "https://clojars.org/repo/" art-path ".pom --no-check-certificate")
   (concat "http://repo1.maven.org/maven2/" art-path ".jar")
   (concat "http://repo1.maven.org/maven2/" art-path ".pom")))

;;;###autoload
(defun clojure-offline-process-artifacts (process artifact-names-array)
  "Create the certain part of the scrpt by each artifact processing.
`process' - function with paramer `artifact-name'"
  (append
   (mapcar (lambda (art) (concat art "\n"))
           (mapcar process artifact-names-array))
   (list "\n")))

;;;###autoload
(defun clojure-offline-get-jar-urls (artifact-name)
  "Convert from maven's `artifact-name' to probably jar and pom url locations
on clojars and maven central.
E.g. convert from
lein-ring:lein-ring:pom:0.8.2
to
https://clojars.org/repo/lein-ring/lein-ring/0.8.2/lein-ring-0.8.2.jar"
  (clojure-offline-with-artifact
   artifact-name
   (let ((art-path (concat (mapconcat 'identity
                                      (split-string group-id "\\.") "/") "/"
                           artifact-id "/" version "/"
                           artifact-id "-" version)))
     (clojure-offline-form-urls art-path))))

;;;###autoload
(defun clojure-offline-get-download-script (artifact-name)
  (let ((download-script
         (apply 'concat
                (mapcar (lambda (art-path)
                          (concat "wget " art-path "\n"))
                        (clojure-offline-get-jar-urls artifact-name)))))
    (substring download-script 0 (1- (length download-script)))))

;;----------------------------------------------------------------------
;;
;;;###autoload
(defun clojure-offline-get-localrepo-install (artifact-name)
  "Convert from maven's `artifact-name' to local maven repository creation
script.
lein localrepo install <filename> <[groupId/]artifactId> <version>

E.g. convert from
lein-ring:lein-ring:pom:0.8.2
to
lein localrepo install foo-1.0.6.jar com.example/foo 1.0.6"
  (clojure-offline-with-artifact
   artifact-name
   (let ((jar-file-name ))
     (concat "lein localrepo install "
             artifact-id "-" version ".jar "
             group-id "/" artifact-id " " version))))

;;----------------------------------------------------------------------
;;
;;;###autoload
(defun clojure-offline-get-file-name (artifact-name extension)
"E.g. (clojure-offline-get-file-name [org.clojure/clojure \"1.5.1\"] \"jar\")"
  (clojure-offline-with-artifact
   artifact-name
   (concat artifact-id "-" version "." extension)))

;;;###autoload
(defun clojure-offline-get-mvn-deploy (artifact-name)
  "Convert from maven's `artifact-name' to local maven repository creation
script.
E.g. convert from
lein-ring:lein-ring:pom:0.8.2
to
mvn deploy:deploy-file -DgroupId=lein-ring -DartifactId=lein-ring \
    -Dversion=0.8.2 -Dpackaging=jar -Dfile=lein-ring-0.8.2.jar \
    -Durl=file:maven_repository"
  (clojure-offline-with-artifact
   artifact-name
   (concat "mvn deploy:deploy-file "
           "-DgroupId=" group-id " "
           "-DartifactId=" artifact-id " "
           "-Dversion=" version " "
           "-Dpackaging=" "jar" " "
           "-Dfile=" (clojure-offline-get-file-name artifact-name "jar") " "
           "-Durl=" "file:maven_repository")))

;;----------------------------------------------------------------------
;;
;;;###autoload
(defun clojure-offline-get-m2-path (artifact-name)
  (clojure-offline-with-artifact
   artifact-name
   (let* ((home (if (equal (file-name-base
                            (directory-file-name (getenv "HOME")))
                           "Application Data")
                    (expand-file-name ".." (getenv "HOME"))
                  (getenv "HOME")))
          (m2 (clojure-offline-concat-path home ".m2" "repository"))
          (sep (if (eq system-type 'windows-nt) "\\\\" "/")))
     (clojure-offline-concat-path m2
                  (replace-regexp-in-string "\\." sep group-id)
                  artifact-id
                  version))))

;;;###autoload
(defun clojure-offline-get-manual-copy (artifact-name)
  (clojure-offline-with-artifact
   artifact-name
   (let ((artifact-m2-path (if (eq system-type 'windows-nt)
                               (replace-regexp-in-string
                                "/" "\\\\"
                                (clojure-offline-get-m2-path artifact-name))
                             (clojure-offline-get-m2-path artifact-name)))
         (artifact-jar-filename
          (clojure-offline-get-file-name artifact-name "jar"))
         (artifact-pom-filename
          (clojure-offline-get-file-name artifact-name "pom")))
     (concat "copy " artifact-jar-filename " \"" artifact-m2-path "\"\n"
             "copy " artifact-pom-filename " \"" artifact-m2-path "\""))))

;;----------------------------------------------------------------------
;;
;;;###autoload
(defun clojure-offline-create-script-buffer ()
  "Create buffer dedicated to output configure required clojure jars."
  (let ((buf (get-buffer-create clojure-offline-script-buffer-name)))
    (save-excursion
      (set-buffer buf)
      (toggle-truncate-lines nil)
      buf)))

(defvar clomacs-loaded nil)

(when (require 'clomacs nil 'noerror)
  (clomacs-defun clojure-offline-get-dependeces
                 clojure-offline.lein-caller/get-dependeces-list
                 :lib-name "clojure-offline"
                 :namespace clojure-offline.lein-caller)
  (setq clomacs-loaded t))

;;;###autoload
(defun* clojure-offline-create-script
    (artifact-names
     &optional &key
     (download t)
     (install :localrepo)
     (dependencies clomacs-loaded)
     (deploy nil)
     (clear t))
  "Construct download and installation script for certain clojure dependencies.
The parameters are:
`download' when t - create required artifacts download script.
`install' `:localrepo' - install artifacts via localrepo leiningen plugin
          `:manual' - install artifacts via simple copy
`dependencies' when t - search for inner dependencies via clojure-side code.
`deploy' when t - creaate maven local repository.
`clear' when t - clear output script buffer from previous output.
"
  (interactive
   (list
    (read-from-minibuffer "Clojure artifacts vector (or single artifact): "
                          (buffer-substring (mark) (point)) nil nil
                          'clojure-offline-artifacts-list-history)))
  (if dependencies
      (clomacs-set-offline t))
  (let* ((artifact-names (if (and (not (vectorp artifact-names))
                                  (stringp artifact-names))
                             (read (clojure-offline-trim-string artifact-names))
                           artifact-names))
         ;; When only single artifact is passed.
         (artifact-names (if (not (sequencep (elt artifact-names 0)))
                             (vector artifact-names)
                           artifact-names))
         ;; For inner dependencies.
         (artifact-names (if dependencies
                             (read
                              (clojure-offline-get-dependeces artifact-names))
                           artifact-names))
         (script (apply 'concat
                        (append
                         ;; Downloads script
                         (if download
                             (clojure-offline-process-artifacts
                              'clojure-offline-get-download-script
                              artifact-names))
                         ;; Copy .jar and .pom files script
                         (if (equal install :localrepo)
                             (clojure-offline-process-artifacts
                              'clojure-offline-get-localrepo-install
                              artifact-names)
                           (if (equal install :manual)
                               (clojure-offline-process-artifacts
                                'clojure-offline-get-manual-copy
                                artifact-names)))
                         ;; Maven deploy
                         (if deploy
                             (clojure-offline-process-artifacts
                              'clojure-offline-get-mvn-deploy
                              artifact-names))))))
    (set-buffer (clojure-offline-create-script-buffer))
    (if (equal clear nil)
        (insert "\n\n")
      (erase-buffer))
    (end-of-buffer)
    (insert script)
    (switch-to-buffer (clojure-offline-create-script-buffer))
    (message "Script created.")))

(provide 'clojure-offline)
