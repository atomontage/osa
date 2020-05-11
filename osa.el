;;; osa.el --- OSA (JavaScript / AppleScript) bridge -*- lexical-binding: t; -*-

;; Copyright (C) 2020 xristos@sdf.org
;; All rights reserved

;; Version: 1.0 - 2020-04-29
;; Author: xristos <xristos@sdf.org>
;; URL: https://github.com/atomontage/osa
;; Package-Requires: ((emacs "25"))
;; Keywords: extensions

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
;; This is an Emacs Lisp to macOS Open Scripting Architecture
;; (JavaScript / AppleScript) bridge, working on top of Apple Event descriptors
;; as provided by Emacs Mac port.
;;
;; Please see README.org for documentation.
;;
;;; TODO:
;;
;; + Support more Apple Event data types (date, enum, float, ..)
;; + Revisit assumptions if necessary (little-endian host, utf-16le default)
;;


;;; Code:

(require 'mac)
(require 'seq)
(require 'subr-x)
(require 'bindat)
(require 'cl-lib)
(require 'cl-generic)

(defvar osa-debug t
  "If non-nil, log unpacking errors to *Messages*.")

(defvar osa-strict-unpacking nil
  "If non-nil, signal errors if an Apple Event descriptor can not be unpacked.
Otherwise, unpacking returns descriptor data as-is, in a cons of form
\(:aedesc . data).")

(cl-defgeneric osa--pack (object)
  "Pack an Emacs Lisp object for use with `mac-osa-script'.

The object is packed into a Lisp representation of an Apple Event descriptor.
The following Emacs Lisp objects are supported:

+ t and nil
+ Integer (must be within signed 32bit range or error is signaled)
+ String (encoded as UTF-16LE)
+ Tagged alist (:reco (k . v) ..)
+ Vector and tagged list (:list ..)
+ :null
+ (:type :null), (:type :msng), (:type data)

Use `osa-pack' rather than directly calling `osa--pack'.")

(cl-defgeneric osa--unpack (type value)
  "Unpack an Apple Event descriptor with TYPE and VALUE to an
Emacs Lisp object. TYPE must be a keyword symbol.

The following Apple Event descriptor types are supported:

+ True is unpacked to t
+ False is unpacked to nil
+ Boolean type is unpacked to t or nil
+ Long is unpacked from signed 32bit to Emacs Lisp integer
+ Null which is distinct from null type, is unpacked to keyword :null
+ Type is unpacked to either (:type :null), (:type :msng), (:type data)
+ Unicode text (UTF-16 BOM auto-detection)
+ List is unpacked to Emacs Lisp vector
+ Record is unpacked to Emacs Lisp tagged alist: (:reco (k .v))

Use `osa-unpack' rather than directly calling `osa--unpack'.")


;;;
;;; Packing
;;;


(cl-defmethod osa--pack ((_object (eql t)))   (cons "true" ""))
(cl-defmethod osa--pack ((_object (eql nil))) (cons "fals" ""))

(cl-defmethod osa--pack ((object integer))
  (cl-assert
   (and object (< object #x80000000) (>= object (- #x80000000)))
   t)
  (let ((u32 (bindat-pack '((u u32r)) `((u . ,object)))))
    (cons "long" u32)))

(cl-defmethod osa--pack ((object string))
  (cons "utxt" (encode-coding-string object 'utf-16le)))

(cl-defmethod osa--pack ((object vector))
  (cl-loop
   with ret
   for elem across object do
   (push (osa--pack elem) ret)
   finally return (cons "list" (nreverse ret))))

(cl-defmethod osa--pack ((object (head :list)))
  (cl-loop
   with ret
   for elem in (cdr object) do
   (push (osa--pack elem) ret)
   finally return (cons "list" (nreverse ret))))

(cl-defmethod osa--pack ((object (head :reco)))
  (cl-loop
   with ret
   for (k . v) in (cdr object) do
   (push (osa--pack k) ret)
   (push (osa--pack v) ret)
   finally return (list "reco"
                        (nconc (list "usrf" "list")
                               (nreverse ret)))))

(cl-defmethod osa--pack ((object (head :type)))
  (pcase (cl-second object)
    (:msng (cons "type" "gnsm"))
    (:null (cons "type" "llun"))
    (other (cl-assert (stringp other) t)
           (cons "type" other))))

(cl-defmethod osa--pack ((_object (eql :null)))
  (cons "null" nil))

;;;###autoload
(defun osa-pack (object)
  "Pack Emacs Lisp OBJECT into an Apple Event Lisp representation.
Return cons of form (type . data) on success.
Errors are signaled otherwise.

Packing is implemented in the generic function `osa--pack'."
  (condition-case-unless-debug err
      (osa--pack object)
    ('error (error "%s when packing %s"
                   (error-message-string err)
                   (prin1-to-string object)))))


;;;
;;; Unpacking
;;;


(cl-defmethod osa--unpack ((_type (eql :true)) (v string))
  (cl-assert (string= v "") t)
  t)

(cl-defmethod osa--unpack ((_type (eql :fals)) (v string))
  (cl-assert (string= v "") t)
  nil)

(cl-defmethod osa--unpack ((_type (eql :null)) (_v null))
  :null)

(cl-defmethod osa--unpack ((_type (eql :bool)) (v string))
  (cl-assert (> (length v) 0) t)
  (/= 0 (aref v 0)))

(cl-defmethod osa--unpack ((_type (eql :type)) (v string))
  (cl-assert (> (length v) 0) t)        ; is it always == 4?
  (pcase v
    ("gnsm" (list :type :msng))
    ("llun" (list :type :null))
    (_      (list :type v))))

(cl-defmethod osa--unpack ((_type (eql :long)) (v string))
  (cl-assert (= (length v) 4) t)
  ;; Unsigned to signed
  (let* ((u32 (bindat-get-field
               (bindat-unpack '((u u32r)) v) 'u)))
    (if (> u32 #x7fffffff) (logior -4294967296 u32) u32)))

(cl-defmethod osa--unpack ((_type (eql :utxt)) (v string))
  (cond ((or (string-prefix-p "\xfe\xff" v)
             (string-prefix-p "\xff\xfe" v))
         (decode-coding-string v 'utf-16))
        (t (decode-coding-string v 'utf-16le))))

(cl-defmethod osa--unpack ((_type (eql :list)) (v list))
  (cl-loop for (k2 . v2) in v
           vconcat (list (osa--unpack k2 v2))))

(cl-defmethod osa--unpack ((_type (eql :reco)) (v list))
  (cl-loop with ret for (tag . rest) in v do
           (cl-assert (stringp tag) t)
           (if (string= tag "usrf")
               (progn
                 (cl-assert (string= "list" (car rest)) t)
                 (cl-loop for (k v) on (cdr rest) by #'cddr do
                          (cl-assert (and k (listp k) v (listp v)) t)
                          (push (cons (osa--unpack (car k) (cdr k))
                                      (osa--unpack (car v) (cdr v)))
                                ret)))
             (push (cons tag (osa--unpack (car rest) (cdr rest)))
                   ret))
           finally return (cons :reco (nreverse ret))))

(cl-defmethod osa--unpack
    :before ((_type symbol) (v string))
    (cl-assert (not (multibyte-string-p v)) t))

(cl-defmethod osa--unpack ((type string) v)
  (cl-assert (> (length type) 0))
  (let ((key (intern-soft (format ":%s" type))))
    (osa--unpack key v)))

;;;###autoload
(defun osa-unpack (aedesc)
  "Unpack Emacs Lisp representation of Apple Event descriptor.
Return Emacs Lisp object on successful parsing or descriptor
data / signal error depending on `osa-strict-unpacking'.

AEDESC must be a cons of form (type . data) as returned from
`mac-osa-script' when passed VALUE-FORM as t.
Unpacking is implemented in the generic function `osa--unpack'.

If `osa-strict-unpacking' is non-nil, errors are signaled on all
unpacking failures. Otherwise original descriptor data is
returned as-is in a cons of form (:aedesc . data)."
  (cl-flet ((format-error (err) (format "%s when unpacking %s"
                                        (error-message-string err)
                                        (prin1-to-string aedesc)))
            (unpack () (osa--unpack (car aedesc) (cdr aedesc))))
    (if osa-strict-unpacking
        (condition-case-unless-debug err (unpack)
          ('error (error "%s" (format-error err))))
      (condition-case err (unpack)
        ('error (when osa-debug
                  (let ((message-truncate-lines t))
                    (message "osa-unpack: %s" (format-error err))
                    (message nil)))
                (cons :aedesc aedesc))))))


;;;
;;; Evaluation
;;;


;;;###autoload
(cl-defun osa-eval (src &key (unpack t) (lang "AppleScript")
                        call args &allow-other-keys)
  "Evaluate SRC through OSA and return the result.

SRC must be AppleScript or JavaScript source code.

If UNPACK is non-nil, result is unpacked into Emacs Lisp objects
through `osa-unpack'. Otherwise, unmodified Lisp representation of
Apple Event descriptor as generated by `mac-osa-script' is returned.

If CALL is present, it must be the name of a handler/function in SRC
to be called with arguments in ARGS list.

All arguments in ARGS must be Emacs Lisp objects that `osa-pack' can pack.

Errors are signaled if evaluation fails."
  (when call (cl-assert (stringp call) t))
  (when args (cl-assert (listp args) t))
  (cl-assert (seq-contains '("AppleScript" "JavaScript") lang) t)
  (let ((src (string-to-multibyte src))
        (args (and call args (cl-mapcar #'osa-pack args))))
    (let ((res (apply #'mac-osa-script src lang nil t call args)))
      (if unpack (osa-unpack res) res))))

;;;###autoload
(cl-defun osa-eval-file (path &rest rest &key (lang "AppleScript")
                              include debug &allow-other-keys)
  "Evaluate contents of PATH through `osa-eval' and return the result.

PATH must be a file containing AppleScript or JavaScript source code.

If INCLUDE is present, it must be a path to a file or a list of paths.
All files specified through INCLUDE are read and their contents prepended,
in the order given, to the contents of PATH, forming one final source string
that is passed to `osa-eval'. INCLUDE is meant to be used with reusable
code that was factored out into library files.

If DEBUG is non-nil, a new buffer is created and the final source string
inserted there, for later reference/review, before `osa-eval' is called.

See `osa-eval' for extra keyword arguments that are passed through as-is.
Errors are signaled if evaluation fails."
  (cl-assert (seq-contains '("AppleScript" "JavaScript") lang) t)
  (cl-flet ((slurp (p) (with-temp-buffer
                         (insert (if (string= lang "JavaScript") "// " "-- "))
                         (insert (format "Sourced from: %s\n" p))
                         (insert-file-contents p)
                         (string-trim-right (buffer-string)))))
    (let ((src (mapconcat #'slurp
                          (append (if (not (listp include))
                                      (list include)
                                    include)
                                  (list path))
                          "\n\n")))
      (when debug
        (with-current-buffer (generate-new-buffer "*osa-eval-file*")
          (insert src)
          (goto-char (point-min))
          (message "osa-eval: %d characters written to buffer %s"
                   (buffer-size) (buffer-name))))
      (apply #'osa-eval src rest))))

(provide 'osa)
;;; osa.el ends here
