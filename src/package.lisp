(defpackage :netgear-repair
  (:use :common-lisp
        :alexandria
        :bordeaux-threads
        :cl-ppcre
        :cl-store
        :curry-compose-reader-macros
        :delta-debug
        :diff
        :elf
        :metabang-bind
        :software-evolution
        :software-evolution-utility
        :split-sequence)
  (:shadowing-import-from :elf :type :magic-number :size))
