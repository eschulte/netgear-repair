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

(defvar 1-evolved (mapcar [#'lastcar #'restore] 1-fix-paths)
  "Un-minimized evolved fixes found using only the exploit tests.")
;; => (store 1-evolved "stuff/1-evolved.store")

(defvar 1-full-fitnesses (let ((tests nil)) (mapcar #'test 1-evolved))
  "Fitness of `1-evolved' using the full regression tests suite.")
;; => (8 22 21 19 6 16 17 20 14 21)

(defvar 1-minimized (mapcar {delta-debug orig} 1-evolved)
  "Minimized evolved fixes found using only the exploit tests.")
;; => (store 1-minimized "stuff/1-minmized.store")

(defvar 1-full-min-fitnesses (let ((tests nil)) (mapcar #'test 1-minimized))
  "Fitness of `1-minimized' using the full regression tests suite.")
