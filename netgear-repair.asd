(defsystem :netgear-repair
  :description "Evolve repairs to exploits in NETGEAR's net-cgi executable."
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               bordeaux-threads
               cl-ppcre
               cl-store
               curry-compose-reader-macros
               delta-debug
               diff
               elf
               metabang-bind
               software-evolution
               software-evolution-utility
               split-sequence)
  :components
  ((:file "src/package")
   (:file "src/netgear-repair" :depends-on ("src/package"))))
