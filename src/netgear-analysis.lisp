;;; netgear-analysis.lisp --- analysis of results of repair

;; Copyright (C) 2013  Eric Schulte

;;; Code:
(in-package :netgear-repair)

(defun index-where (pred sequence)
  (loop :for el :in sequence :as i :from 0 :when (funcall pred el) :collect i))

(defun differences (seq1 seq2)
  (loop :for a :in seq1 :for b :in seq2 :as i :from 0
     :unless (equalp a b) :collect (list i a b)))


;;; 1-fixes
(defvar 1-fix-paths (list
                     "checkpoints/interactive/1-0/90405-fixes.store"
                     "checkpoints/interactive/1-1/17231-fixes.store"
                     "checkpoints/interactive/1-2/26879-fixes.store"
                     "checkpoints/interactive/1-3/23764-fixes.store"
                     "checkpoints/interactive/1-4/47906-fixes.store"
                     "checkpoints/interactive/1-5/13102-fixes.store"
                     "checkpoints/interactive/1-6/76960-fixes.store"
                     "checkpoints/interactive/1-7/11831-fixes.store"
                     "checkpoints/interactive/1-8/2846-fixes.store"
                     "checkpoints/interactive/1-9/25600-fixes.store")
  "Paths to evolved fixes found using only the exploit tests.")

(defvar 1-fix-evals
  (mapcar (lambda (path)
            (multiple-value-bind (matched-p matches)
                (scan-to-strings "/([0-9]+)-fixes" path)
              (parse-integer (aref matches 0))))
          1-fix-paths))
;; => (90405 17231 26879 23764 47906 13102 76960 11831 2846 25600)

(defvar 1-evolved (restore "stuff/1-evolved.store")
  "Un-minimized evolved fixes found using only the exploit tests.")
;; <= (mapcar [#'lastcar #'restore] 1-fix-paths)

(defvar 1-full-fitnesses '(8 22 21 19 6 16 17 20 14 21)
  "Fitness of `1-evolved' using the full regression tests suite.")
;; <= (let ((tests nil)) (mapcar #'test 1-evolved))

(defvar 1-diff-sizes '(500 134 205 199 319 95 556 79 10 182)
  "Number of unified diff windows of `1-evolved' against the original.")

(defvar 1-minimized (restore "stuff/1-minimized.store")
  "Minimized evolved fixes found using only the exploit tests.")
;; <= (mapcar {delta-debug orig} 1-evolved)

(defvar 1-full-min-fitnesses '(22 22 22 22 6 22 22 22 14 22)
  "Fitness of `1-minimized' using the full regression tests suite.")
;; <= (let ((tests nil)) (mapcar #'test 1-minimized))

(defvar 1-min-dif-sizes '(2 3 2 2 2 2 3 3 1 2)
  "Number of unified diff windows of `1-minimized' against the original.")
;; <= (mapcar [#'length #'diff-windows
;;             {generate-seq-diff 'unified-diff (lines orig)} #'lines]
;;            1-minimized)

(defvar successful-minimized-edit-locations
  '(325719 329309 33186 33187 33188 33189 33190 33191 332188 332588 17274 329308
    17610 17608 426575 83238 292601 32519 83389 426593 30303 426574)
  "Locations of edits in successful minimized repairs.")
;; <= (remove-duplicates
;;       (mapcan [{mapcar #'car} {differences (lines orig)} #'lines]
;;               (mapcar #'car
;;                       (remove-if-not [{= 22} #'second]
;;                                      (mapcar #'list
;;                                              1-minimized
;;                                              1-full-min-fitnesses)))))


;;; 2-fixes
(defvar 2-fix-paths (list
                     "checkpoints/interactive/2-0/17459-fixes.store"
                     "checkpoints/interactive/2-1/78262-fixes.store")
  "Paths to evolved fixes found using only the exploit tests.")

(defvar 2-fix-evals
  (mapcar (lambda (path)
            (multiple-value-bind (matched-p matches)
                (scan-to-strings "/([0-9]+)-fixes" path)
              (parse-integer (aref matches 0))))
          2-fix-paths))
;; => (17459 78262)

(defvar 2-evolved (restore "stuff/2-evolved.store")
  "Un-minimized evolved fixes found using only the exploit tests.")
;; <= (mapcar [#'lastcar #'restore] 2-fix-paths)

(defvar 2-full-fitnesses '(16 22)
  "Fitness of `2-evolved' using the full regression tests suite.")
;; <= (let ((tests nil)) (mapcar #'test 2-evolved))

(defvar 2-diff-sizes '(123 585)
  "Number of unified diff windows of `2-evolved' against the original.")

(defvar 2-minimized (restore "stuff/2-minimized.store")
  "Minimized evolved fixes found using only the exploit tests.")
;; <= (mapcar {delta-debug orig} 2-evolved)

(defvar 2-full-min-fitnesses '(20 22)
  "Fitness of `2-minimized' using the full regression tests suite.")
;; <= (let ((tests nil)) (mapcar #'test 2-minimized))

(defvar 2-min-dif-sizes '(4 2)
  "Number of unified diff windows of `2-minimized' against the original.")
;; <= (mapcar [#'length #'diff-windows
;;             {generate-seq-diff 'unified-diff (lines orig)} #'lines]
;;            2-minimized)


;;; Test suite coverage and timing
(defvar test-suites '(("BRS-authentication"
                       "securityquestions.cgi-authentication"
                       "unauth.cgi-authentication")
                      ("center_language.html"
                       "BRS-authentication"
                       "securityquestions.cgi-authentication"
                       "unauth.cgi-authentication")
                      ("advanced.js"
                       "center_language.html"
                       "base.gif"
                       "securityquestions.cgi"
                       "BRS-authentication"
                       "securityquestions.cgi-authentication"
                       "unauth.cgi-authentication")
                      ("advanced.js"
                       "logo.jpg"
                       "center_language.html"
                       "Add_WPS_Client.htm"
                       "base.gif"
                       "dtree.css"
                       "securityquestions.cgi"
                       "unauth.cgi"
                       "unauth.cgi-authentication"
                       "securityquestions.cgi-authentication"
                       "BRS-authentication"))
  "Test suites of varying size.")

(defvar runtimes '(3297119/1000 3758629/1000 5766221/1000 981568/125)
  #+(or )
  (mapcar
   (lambda (my-tests)
     (setf tests my-tests)
     (let ((start (get-internal-real-time)))
       (loop :for i :below 1000 :do
          (test (mutate (copy orig))))
       (/ (- (get-internal-real-time) start)
          internal-time-units-per-second)))
   test-suites)
  "Runtime for each test suite.")

(defvar annotated
  (mapcar
   (lambda (samples) (annotate (copy orig) samples))
   (mapcar
    [#'read-sample-file {format nil "results/suite-coverage/tests-~d"} #'length]
    test-suites))
  "Copies of the original elf file annotated with each test suite samples.")

(defvar coverages
  (mapcar [{index-where {aget :trace}} {coerce _ 'list} #'genome] annotated)
  "Locations of trace data for each test suite.")

(with-open-file (out "results/suite-coverage/coverage-by-runtime.txt"
                     :direction :output)
  (mapc {format out "~{~{~a~^ ~}~^~%~}~%"}
        (mapcar (lambda (runtime coverage)
                  (mapcar {list (float (/ runtime 1000))} coverage))
                runtimes coverages)))
