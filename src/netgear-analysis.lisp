;;; netgear-analysis.lisp --- analysis of results of repair

;; Copyright (C) 2013  Eric Schulte

(in-package :netgear-repair)

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
