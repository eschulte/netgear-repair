;;; netgear-repair.lisp --- evolve repairs to NETGEAR exploits

;; Copyright (C) 2013  Eric Schulte

;;; Code:
(in-package :netgear-repair)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

;; The NETGEAR firmware is big endian MIPS.
(setf elf:*endian* 'big)

(defvar orig nil
  "A software object holding the original broken net-cgi file.")

(defvar baseline-fitness 19
  "Fitness of the original program (when running all tests).")

(defvar target-fitness 22
  "Target fitness (when running all tests).")

(defvar fixes nil "List to hold fixes.")

(defvar *port* 6600
  "Set to the port number of the VM.")

(defvar number-of-threads 64
  "The number of threads in which to run repair.")

(defvar threads nil
  "List to hold all running threads.")

(defvar tests nil
  "Optional list of which tests to run.  If nil then run all tests.")

(defvar *results-dir* "checkpoints"
  "Directory in which to write results.")

(defun test (cgi)
  "Test CGI and return it's fitness."
  (with-temp-file (bin)
    (multiple-value-bind (path exit) (phenome cgi :bin bin)
      (declare (ignorable path))
      (unless (zerop exit) (return-from test 0)))
    (multiple-value-bind (stdout stderr errno)
        (shell "bin/test-vm -p ~d ~a ~{~a~^ ~}" *port* bin tests)
      (declare (ignorable stderr) (ignorable errno))
      (reduce (lambda-bind (acc (type multiplier))
                (+ acc (* multiplier
                          (count-if
                           [{string= type} #'car {split-sequence #\Space}]
                           (split-sequence #\Newline stdout)))))
              '(("PASS"  2)
                ("FAIL"  1)
                ("ERROR" 0))
              :initial-value 0))))

(defvar checkpoint-counter 0)
(defun checkpoint ()
  "Function used to checkpoint progress of the evolutionary search."
  ;; write out population stats
  (with-open-file (out (format nil "~a/stats.txt" *results-dir*)
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (format out "~&~{~a~^ ~}~%"
            (cons *fitness-evals* (mapcar #'fitness *population*))))
  (incf checkpoint-counter)
  #+save-pops
  (when (zerop (mod checkpoint-counter 32))
    ;; store the whole population
    (store (mapcar #'genome *population*)
           (format nil "checkpoints/~d-population.store" *fitness-evals*)))
  (when (zerop (mod checkpoint-counter 8))
    ;; store the best individual
    (let ((best (extremum *population* #'> :key #'fitness)))
      (store best (format nil "~a/~d-best-~d.store"
                          *results-dir* *fitness-evals* (fitness best))))))


;;; Annotation
(defun read-sample-file (file)
  "Read a sample file generated with opreport."
  ;; do the initial munging in the shell
  (let ((stdout (shell "cat ~a|grep \"^ *0\"|awk '{print $1,$2}'|sed 's/00/#x/'"
                       file)))
    (mapcar [{mapcar #'read-from-string} {split-sequence #\Space}]
            (cdr (split-sequence #\Newline stdout :remove-empty-subseqs t)))))

(defun annotate (elf samples)
  "Annotate ELF with the samples in SAMPLE-FILE."
  (with-slots (genome base) elf
    ;; convert samples into offsets in the genome
    (let* ((genome-offset 0)
           (loadable (remove-if-not [{eql :load}  #'elf:type] (sections base)))
           (base-address (extremum (mapcar [#'vaddr #'ph] loadable) #'<)))
      ;; annotate the genome of the elf object with annotated files
      (mapc (lambda (sec)
              (let ((mem-adj (- (vaddr (ph sec)) base-address)))
                (mapc (lambda-bind ((address trace))
                        (let ((sec-index (- address mem-adj)))
                          (when (and (< 0 sec-index) (< sec-index (size sec)))
                            (push (cons :trace trace)
                                  (aref genome (+ genome-offset sec-index))))))
                      samples)
                (incf genome-offset (size sec))))
            loadable))))

(defun use-annotation ()
  "This tells the mutation operators how to use annotations to bias
the selection of points in the genome as targets for mutation."
  (defmethod pick-bad ((elf elf-mips-sw))
    (proportional-pick (coerce (genome elf) 'list)
                       (lambda (line) (+ 0.5 (or (aget :trace line) 0))))))


;;; Evolution
(defun run ()
  ;; Use the sh-runner to run shell scripts
  ;; https://github.com/eschulte/sh-runner
  (setf *work-dir* "sh-runner/work")

  ;; Sanity check
  (setf orig (from-file (make-instance 'elf-mips-sw) "stuff/net-cgi"))
  (unless (fitness orig) (setf (fitness orig) (test orig)))
  (assert (= (fitness orig) baseline-fitness) (orig)
          "Original program does not have baseline fitness! (~d/~d)"
          (fitness orig) baseline-fitness)

  ;; Annotate the ELF file with our oprofile samples
  ;; (annotate orig (read-sample-file "stuff/net-cgi.sample"))

  ;; Build the population
  (setf *max-population-size* (expt 2 9))
  (setf *population*
        (loop :for i :below (/ *max-population-size* 2) :collect (copy orig)))

  ;; Launch evolution threads
  (loop :for n :below number-of-threads :do
     (let ((name (format nil "worker-~d" n)))
       (push
        (make-thread
         (lambda ()
           (let ((*port* (+ 6600 n)))
             (push
              (evolve
               #'test
               :target target-fitness            ; stop when passes all tests
               :filter [#'not #'zerop #'fitness] ; ignore broken mutants
               :period (expt 2 4)                ; record keeping
               :period-fn #'checkpoint)
              fixes)
             (setf *running* nil)
             (sleep 120)                ; kill peers after 2 min
             (mapc #'destroy-thread
                   (remove-if [{string= name} #'thread-name]
                              threads))))
         :name (format nil "worker-~d" n))
        threads)))

  (make-thread (lambda ()
                 (ignore-errors (mapc #'join-thread threads))
                 (store fixes (format nil "~a/~d-fixes.store"
                                      *results-dir* *fitness-evals*)))
               :name "cleanup"))

(defun run-many (&key (from 0) (below 10) (prefix 1))
  "Run multiple iterations of `run'."
  (loop :for run :from from :below below :do
     (setf *results-dir* (format nil "checkpoints/interactive/~d-~d" prefix run)
           fixes nil
           *population* nil
           *fitness-evals* 0
           threads nil)
     (join-thread (run))
     (store `((:best          . ,(lastcar fixes))
              (:fitness-evals . ,*fitness-evals*)
              (:population    . ,*population*))
            (format nil "~a/summary.store" *results-dir*))))


;;; Delta Debugging
(defun delta-debug (orig new)
  "Use Delta Debugging to minimize the genetic difference between ORIG
and NEW while maintaining their phenotypic differences"
  ;; sanity check
  (let ((fit (test orig)))
    (unless (= fit baseline-fitness)
      (error "Original program has bad fitness!~%~S" fit)))
  ;; calculate the original difference
  (let* ((base (lines orig))
         (diff (generate-seq-diff 'unified-diff base (lines new))))
    (format t "minimizing ~d diff windows~%" (length (diff-windows diff)))
    (flet ((from-windows (windows)
             ;; build a new individual from a set of diff windows
             (let ((new (copy orig)))
               (setf (lines new)
                     (car (reduce
                           (lambda-bind ((seq offset) window)
                             (multiple-value-call #'list
                               (apply-seq-window seq window :offset offset)))
                           windows :initial-value (list base 0))))
               new)))
      ;; return the minimized individual
      (from-windows (minimize (diff-windows diff)
                              (lambda (windows)
                                (ignore-errors
                                  (= target-fitness
                                     (test (from-windows windows))))))))))
