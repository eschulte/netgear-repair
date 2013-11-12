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
    (multiple-value-bind (stdout stderr errno) (shell "bin/test-vm ~a" bin)
      (declare (ignorable stderr) (ignorable errno))
      (count-if [{string= "PASS"} #'car {split-sequence #\Space}]
                (split-sequence #\Newline stdout)))))

#+running
(
;; Sanity check
(setf orig (from-file (make-instance 'elf-mips-sw) "stuff/net-cgi")
      (fitness orig) (test orig))
(assert (= (fitness orig) 7) (orig)
        "Original program does not pass all regression tests! (~d/7)"
        (fitness orig))

;; Build the population
(setf *population*
      (loop :for i :below *max-population-size* :collect (copy orig)))

;; Launch all threads
(dotimes (n number-of-threads threads)
  (push (make-thread (lambda () (evolve #'test :target 10))
                     :name (format nil "worker-~d" n))
        threads))
)
