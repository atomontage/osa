;;; osa-test.el --- osa.el tests -*- lexical-binding: t; -*-

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
;; Standalone (command-line) testing:
;;
;; emacs -L . --batch -l osa-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'osa)


(ert-deftest osa-unpack-unimplemented ()
  (should-error (osa--unpack "unknown" nil))
  (should-error (osa--unpack :unknown nil))
  (should-error (osa--unpack :unknown "")))

(ert-deftest osa-unpack-true ()
  (should-error (osa--unpack :true nil))
  (should (osa--unpack :true "")))

(ert-deftest osa-unpack-false ()
  (should-error (osa--unpack :fals nil))
  (should-not (osa--unpack :fals "")))

(ert-deftest osa-unpack-null ()
  (should-error (osa--unpack :null ""))
  (should-not (null (osa--unpack :null nil)))
  (should (eq :null (osa--unpack :null nil))))

(ert-deftest osa-unpack-boolean ()
  (should-error (osa--unpack :bool nil))
  (should (osa--unpack :bool "\xff"))
  (should (osa--unpack :bool "\xaa\xbb"))
  (should-not (osa--unpack :bool "\x00"))
  (should-not (osa--unpack :bool "\x00\xff")))

(ert-deftest osa-unpack-type ()
  (should-error (osa--unpack :type nil))
  (should (equal '(:type :msng) (osa--unpack :type "gnsm")))
  (should (equal '(:type :null) (osa--unpack :type "llun")))
  (should (equal '(:type "unknown") (osa--unpack :type "unknown"))))

(ert-deftest osa-unpack-long ()
  (let ((n1   "\xff\xff\xff\xff")
        (zero "\x00\x00\x00\x00")
        (max  "\xff\xff\xff\x7f")
        (min  "\x00\x00\x00\x80"))
    (should-error (osa--unpack :long nil))
    (should (= -1 (osa--unpack :long n1)))
    (should (=  0 (osa--unpack :long zero)))
    (should (= (1- (expt 2 31)) (osa--unpack :long max)))
    (should (= -2130706433 (osa--unpack :long "\xff\xff\xff\x80")))
    (should (= (- (expt 2 31)) (osa--unpack :long min)))
    (should (= -128 (osa--unpack :long "\x80\xff\xff\xff")))
    (should-not (= (osa--unpack :long n1)
                   (osa--unpack :long max)))))

(ert-deftest osa-unpack-string ()
  (let ((t1 "\u00ff\u00ff")
        (t2 "\u0000")
        (t3 "test")
        (no-bom "\x74\x00\x65\x00\x73\x00\x74\x00")
        (be-with-bom "\xfe\xff\x00\x74\x00\x65\x00\x73\x00\x74")
        (le-with-bom "\xff\xfe\x74\x00\x65\x00\x73\x00\x74\x00"))
    (should (string= "" (osa--unpack :utxt "")))
    (should (string= t1 (osa--unpack :utxt "\xff\x00\xff\x00")))
    (should (string= t2 (osa--unpack :utxt "\x00\x00")))
    (should (string= t3 (osa--unpack :utxt no-bom)))
    (should (string= t3 (osa--unpack :utxt be-with-bom)))
    (should (string= t3 (osa--unpack :utxt le-with-bom)))))

(ert-deftest osa-unpack-list ()
  (should-error (osa--unpack :list '(())))
  (should-error (osa--unpack :list '("")))
  (should (equal [] (osa--unpack :list nil)))
  (should (equal [t nil t]
                 (osa--unpack
                  :list '(("true" . "") ("fals" . "") ("true" . "")))))
  (should (equal [nil [t] nil]
                 (osa--unpack
                  :list '(("fals" . "") ("list" ("true" . "")) ("fals" . "")))))
  (should (equal [[[t]]]
                 (osa--unpack :list '(("list" ("list" ("true" . ""))))))))

(ert-deftest osa-unpack-record ()
  (should (equal '(:reco) (osa--unpack :reco nil)))
  (should (equal '(:reco) (osa--unpack :reco '(("usrf" "list")))))
  (should (equal '(:reco ("key" . t))
                 (osa--unpack :reco '(("key" "true" . "")))))
  (should (equal '(:reco ("key" . []))
                 (osa--unpack :reco '(("key" "list")))))
  (should (equal '(:reco ("key" . t)
                           ((:type :msng) . (:reco))
                           ((:reco) . :null))
                 (osa--unpack :reco
                              '(("key" "true" . "")
                                ("usrf" "list"
                                 ("type" . "gnsm")
                                 ("reco" ("usrf" "list"))
                                 ("reco" ("usrf" "list")) ("null"))))))
  (should (equal '(:reco ("reco" :reco ("key" . t)))
                 (osa--unpack :reco
                              '(("reco" "reco" ("key" "true" . "")))))))


;;;
;;; Packing
;;;


(ert-deftest osa-pack-true ()
  (should (equal '("true" . "") (osa--pack t))))

(ert-deftest osa-pack-false ()
  (should (equal '("fals" . "") (osa--pack nil))))

(ert-deftest osa-pack-null ()
  (should (equal '("null") (osa--pack :null))))

(ert-deftest osa-pack-integer ()
  (let ((n1   "\xff\xff\xff\xff")
        (zero "\x00\x00\x00\x00")
        (max  "\xff\xff\xff\x7f")
        (min  "\x00\x00\x00\x80"))
   (should (equal `("long" . ,zero) (osa--pack 0)))
   (should (equal `("long" . ,n1)   (osa--pack -1)))
   (should (equal `("long" . ,max)  (osa--pack (1- (expt 2 31)))))
   (should (equal `("long" . ,min)  (osa--pack (- (expt 2 31)))))
   (should (equal '("long" . "\xff\xff\xff\x80")
                  (osa--pack -2130706433)))))

(ert-deftest osa-pack-string ()
  (should (equal '("utxt" . "") (osa--pack "")))
  (should (equal '("utxt" . "\x74\x00\x65\x00\x73\x00\x74\x00")
                 (osa--pack "test")))
  (should (equal '("utxt" . "\xff\x00\xff\x00")
                 (osa--pack "\u00ff\u00ff")))
  (should (equal '("utxt" . "\x00\x00")
                 (osa--pack "\u0000"))))

(ert-deftest osa-pack-vector ()
  (should (equal '("list") (osa--pack [])))
  (should (equal '("list" ("list" ("list")))
                 (osa--pack [[[]]])))
  (should (equal '("list" ("true" . "")
                   ("list" ("true" . "") ("fals" . ""))
                   ("fals" . "") ("list"))
                 (osa--pack [t [t nil] nil []]))))

(ert-deftest osa-pack-list ()
  (should (equal '("list") (osa--pack '(:list))))
  (should (equal '("list" ("list" ("list")))
                 (osa--pack '(:list (:list (:list))))))
  (should (equal '("list" ("true" . "")
                   ("list" ("true" . "") ("fals" . ""))
                   ("fals" . "") ("list"))
                 (osa--pack '(:list t (:list t nil) nil (:list))))))

(ert-deftest osa-pack-type ()
  (should (equal '("type" . "llun") (osa--pack '(:type :null))))
  (should (equal '("type" . "gnsm") (osa--pack '(:type :msng))))
  (should (equal '("type" . "unkn") (osa--pack '(:type "unkn")))))

(ert-deftest osa-pack-record ()
  (should-error (osa--pack '(1)))
  (should (equal '("reco" ("usrf" "list"))
                 (osa--pack '(:reco))))
  (should (equal '("reco" ("usrf" "list"
                           ("utxt" . "\x6b\x00\x65\x00\x79\x00")
                           ("true" . "")))
                 (osa--pack '(:reco ("key" . t)))))
  (should (equal '("reco" ("usrf" "list"
                           ("true" . "")
                           ("list")))
                 (osa--pack '(:reco (t . []))))))


;;;
;;; Evaluation
;;;


(ert-deftest osa-eval-javascript ()
  (should (equal :null (osa-eval "" :lang "JavaScript")))
  (should (equal `[t nil ,(1- (expt 2 31))]
                 (osa-eval "function foo(a,b,c) {return [a, b, c];}"
                           :lang "JavaScript"
                           :call "foo"
                           :args `(t nil ,(1- (expt 2 31))))))
  (should (equal [1 t "test" [nil]]
                 (osa-eval "[1, true, 'test', [false]]"
                           :lang "JavaScript")))
  ;; When using JavaScript, integer keys will be converted to strings
  (should (equal '(:reco ("333" . 333))
                 (osa-eval "function foo(a) {return a;}"
                           :lang "JavaScript"
                           :call "foo"
                           :args '((:reco (333 . 333)))))))

(ert-deftest osa-eval-applescript ()
  ;; When using AppleScript, keys have to be strings
  (should-error (osa-eval "{1:true}"))
  (should (equal [1 t "test" [nil]]
                 (osa-eval "{1, true, \"test\", {false}}")))
  (should (equal [[[]]] (osa-eval "{{{}}}")))
  (should (equal [[[(:type :msng)]]]
                 (osa-eval "on foo(a)\nreturn {{{a}}}\nend foo"
                           :call "foo" :args '((:type :msng)))))
  (should (equal :null (osa-eval "return")))
  (should (equal :null (osa-eval ""))))

(defun osa-test ()
  "Run OSA test suite through ERT."
  (interactive)
  (ert-run-tests-interactively "^osa-"))

(provide 'osa-test)
;;; osa-test.el ends here
