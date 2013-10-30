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

;; `clojure-offline-create-script' - create installation script for certain
;; dependencies.
;;
;; See README.md for detail description.

(defvar clojure-offline-script-buffer-name "*clojure-offline*")

(defun clojure-offline-trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string
   "\\`[ \t\n]*" ""
   (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

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
(defun clojure-offline-form-urls (art-path)
  (concat "https://clojars.org/repo/" art-path ".jar" "\n"
          "https://clojars.org/repo/" art-path ".pom" "\n"
          "http://repo1.maven.org/maven2/" art-path ".jar" "\n"
          "http://repo1.maven.org/maven2/" art-path ".pom"))

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

(defun clojure-offline-get-list-clojars-url (artifact-names-array)
  (map 'list 'clojure-offline-get-jar-urls artifact-names-array))

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

(defun clojure-offline-get-list-localrepo-install (artifact-names-array)
  (map 'list 'clojure-offline-get-localrepo-install artifact-names-array))

;;----------------------------------------------------------------------
;;
(defun clojure-offline-get-file-name (artifact-name extension)
  (clojure-offline-with-artifact
   artifact-name
   (concat artifact-id "-" version "." extension)))

(clojure-offline-get-file-name [org.clojure/clojure "1.5.1"] "jar")

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

(defun clojure-offline-get-list-mvn-deploy (artifact-names-array)
  (map 'list 'clojure-offline-get-mvn-deploy artifact-names-array))

;;----------------------------------------------------------------------
;;
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

(defun clojure-offline-get-list-manual-copy (artifact-names-array)
  (map 'list 'clojure-offline-get-manual-copy artifact-names-array))

;;----------------------------------------------------------------------
;;
;;;###autoload
(defun clojure-offline-guess-clojars-url (artifact-name)
  (interactive
   (list
    (read-from-minibuffer "Clojure artifact: "
                          (buffer-substring (mark) (point)) nil nil
                          'clojure-offline-artifact-name-history)))
  (message (clojure-offline-get-jar-urls artifact-name)))

(defun clojure-offline-create-script-buffer ()
  "Create buffer dedicated to output configure required clojure jars."
  (let ((buf (get-buffer-create clojure-offline-script-buffer-name)))
    (save-excursion
      (set-buffer buf)
      (toggle-truncate-lines nil)
      buf)))

;;;###autoload
(defun clojure-offline-create-script
  (artifact-names-array &optional install clear)
  "
`install': nil, 'maven
`clear': nil, t"
  (interactive
   (list
    (read-from-minibuffer "Clojure artifacts list: "
                          (buffer-substring (mark) (point)) nil nil
                          'clojure-offline-artifacts-list-history)))
  (let ((artifact-names-array (if (and (not (vectorp artifact-names-array))
                                       (stringp artifact-names-array))
                                  (read (clojure-offline-trim-string artifact-names-array))
                                artifact-names-array)))
    (set-buffer (clojure-offline-create-script-buffer))
    (if (equal clear nil)
        (insert "\n\n")
      (erase-buffer))
    (end-of-buffer)
    ; Downloads script
    (mapc
     (lambda (art) (mapc
               (lambda (a) (insert (concat "wget " a "\n")))
               (split-string art)))
     (clojure-offline-get-list-clojars-url artifact-names-array))
    ; Copy jars script
    (insert "\n")
    (if (and (equal install nil) (equal install 'maven))
        (mapc (lambda (art) (insert (concat art "\n")))
              (clojure-offline-get-list-mvn-deploy artifact-names-array))
      (mapc (lambda (art) (insert (concat art "\n")))
            (clojure-offline-get-list-localrepo-install
             artifact-names-array)))
    ; Extract *.pom script
    (insert "\n")
    (mapc (lambda (art) (insert (concat art "\n")))
              (clojure-offline-get-list-manual-copy artifact-names-array))
    (switch-to-buffer (clojure-offline-create-script-buffer))))

(provide 'clojure-offline)
