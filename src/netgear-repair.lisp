;;; netgear-repair.lisp --- evolve repairs to NETGEAR exploits

;; Copyright (C) 2013  Eric Schulte

;;; Code:
(in-package :netgear-repair)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

;; The NETGEAR firmware is big endian MIPS.
(setf elf:*endian* 'big)

(defvar orig (from-file (make-instance 'elf-mips-sw) "stuff/net-cgi")
  "A software object holding the original broken net-cgi file.")
