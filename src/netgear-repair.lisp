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

(defvar fixes nil "List to hold fixes.")

(defvar *port* 6600
  "Set to the port number of the VM.")

(defvar number-of-threads 32
  "The number of threads in which to run repair.")

(defvar threads nil
  "List to hold all running threads.")

(defun test (cgi)
  "Test CGI and return it's fitness."
  (with-temp-file (bin)
    (multiple-value-bind (path exit) (phenome cgi :bin bin)
      (declare (ignorable path))
      (unless (zerop exit) (return-from test 0)))
    (multiple-value-bind (stdout stderr errno)
        (shell "bin/test-vm -p ~d ~a" *port* bin)
      (declare (ignorable stderr) (ignorable errno))
      (count-if [{string= "PASS"} #'car {split-sequence #\Space}]
                (split-sequence #\Newline stdout)))))

(defvar checkpoint-counter 0)
(defun checkpoint ()
  "Function used to checkpoint progress of the evolutionary search."
  ;; write out population stats
  (with-open-file (out "checkpoints/stats.txt"
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (format out "~&~{~a~^ ~}~%"
            (cons *fitness-evals* (mapcar #'fitness *population*))))
  (incf checkpoint-counter)
  (when (zerop (mod checkpoint-counter 32))
    ;; store the whole population
    (store (mapcar #'genome *population*)
           (format nil "checkpoints/~d-population.store" *fitness-evals*)))
  (when (zerop (mod checkpoint-counter 8))
    ;; store the best individual
    (let ((best (extremum *population* #'> :key #'fitness)))
      (store best (format nil "checkpoints/~d-best-~d.store"
                          *fitness-evals* (fitness best))))))

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

#+annotating
(
;; This tells the mutation operators how to use annotations to bias
;; the selection of points in the genome as targets for mutation.
(defmethod pick-bad ((elf elf-mips-sw))
  (proportional-pick (coerce (genome elf) 'list)
                     (lambda (line) (+ 0.5 (or (aget :trace line) 0)))))
)

#+running
(
;; Use the sh-runner to run shell scripts
(setf *work-dir* "sh-runner/work")

;; Sanity check
(setf orig (from-file (make-instance 'elf-mips-sw) "stuff/net-cgi"))
(setf (fitness orig) (test orig))
(assert (= (fitness orig) 7) (orig)
        "Original program does not pass all regression tests! (~d/7)"
        (fitness orig))

;; Annotate the ELF file with our oprofile samples
;; (annotate orig (read-sample-file "stuff/net-cgi.sample"))

;; Build the population
(setf *max-population-size* (expt 2 9))
(setf *population*
      (loop :for i :below (/ *max-population-size* 2) :collect (copy orig)))

;; Launch all threads
(loop :for n :below number-of-threads :do
   (push (make-thread
          (lambda ()
            (let ((*port* (+ 6600 n)))
              (push
               (evolve #'test
                :target 10                        ; stop when passes all tests
                :filter [#'not #'zerop #'fitness] ; ignore broken mutants
                :period (expt 2 4)                ; record keeping
                :period-fn #'checkpoint)
               fixes)))
          :name (format nil "worker-~d" n))
         threads))

(make-thread (lambda ()
               (mapc #'join-thread threads)
               (store fixes (format nil "checkpoints/~d-fixes.store"
                                    *fitness-evals*)))
             :name "cleanup")
)
