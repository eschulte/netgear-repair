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

(defvar *port* 2222
  "Set to the port number of the VM.")

(defvar number-of-threads 64
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

(defun checkpoint ()
  "Function used to checkpoint progress of the evolutionary search."
  ;; store the whole population
  (store *population*
         (format nil "checkpoints/~d-population.store" *fitness-evals*))
  ;; store the best individual
  (let ((best (extremum *population* #'> :key #'fitness)))
    (store best (format nil "checkpoints/~d-best-~d.store"
                        *fitness-evals* (fitness best)))))

#+running
(
;; Use the sh-runner to run shell scripts
(setf *work-dir* "sh-runner/work")

;; Sanity check
(setf orig (from-file (make-instance 'elf-mips-sw) "stuff/net-cgi"))
(setf *port* 6600)
(setf (fitness orig) (test orig))
(assert (= (fitness orig) 7) (orig)
        "Original program does not pass all regression tests! (~d/7)"
        (fitness orig))

;; Build the population
(setf *max-population-size* (expt 2 8))
(setf *population*
      (loop :for i :below *max-population-size* :collect (copy orig)))

;; Launch all threads
(loop :for n :from 1 :below number-of-threads :do
   (push (make-thread (lambda () (let ((*port* (+ 6600 n)))
                              (evolve #'test
                                      :target 10
                                      :period (expt 2 10)
                                      :period-fn #'checkpoint)))
                      :name (format nil "worker-~d" n))
         threads))
)
