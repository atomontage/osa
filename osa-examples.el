;;; osa-examples.el --- Examples for osa.el -*- lexical-binding: t; -*-

;; Copyright (C) 2020 xristos@sdf.org
;; All rights reserved

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;
;;   * Redistributions in binary form must reproduce the above
;;     copyright notice, this list of conditions and the following
;;     disclaimer in the documentation and/or other materials
;;     provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;;

;;; Code:

(require 'osa)
(require 'subr-x)
(require 'cl-lib)

(defvar osa-examples-script-directory
  (and load-file-name
       (concat (file-name-directory load-file-name)
               (file-name-as-directory "scripts")))
  "Directory that contains example scripts.
Set this manually if auto-detection fails.")

(defun osa-examples-plist-to-record (plist)
  (cl-loop for (k v) on plist by #'cddr
           collect (cons (string-remove-prefix ":" (symbol-name k))
                         v)
           into ret
           finally return (cons :reco ret)))

(defun osa-examples--find (script-file)
  (unless osa-examples-script-directory
    (error "Script directory is unset (osa-examples-script-directory)"))
  (concat osa-examples-script-directory script-file))

;;;###autoload
(cl-defun osa-examples/notify (msg &rest rest &key title subtitle sound)
  "Display a macOS notification.

MSG is required, all other arguments are optional.
SOUND should be the base name of a file present in:

/System/Library/Sounds/"
  ;; Example: (osa-examples/notify "This is only a test!" :sound "Ping")
  (osa-eval-file (osa-examples--find "notify.js")
                 :lang "JavaScript"
                 :call "notify"
                 :args (list msg (osa-examples-plist-to-record rest))))

;;;###autoload
(defun osa-examples/finder (path)
  "Reveal PATH in a new Finder window.
Return PATH."
  (let ((path (expand-file-name path)))
    (osa-eval-file (osa-examples--find "show-finder.applescript")
                   :call "show_finder" :args (list path))))

;;;###autoload
(defun osa-examples/terminal (path)
  "Open a new Terminal.app window and cd to PATH.

If PATH is a file, cd to its parent directory instead.
Return used path."
  (let ((path (expand-file-name path)))
    (osa-eval-file (osa-examples--find "terminal.applescript")
                   :call "terminal" :args (list path))))


(provide 'osa-examples)
;;; osa-examples.el ends here
