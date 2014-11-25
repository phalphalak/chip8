(ns chip8.core
  (:require [clojure.java.io :refer [file input-stream]])
;  (:import [java.io File])
  )

(defn file->bytes [f]
  (with-open [bis (input-stream f)]
    (let [length (.length (file f))
          buffer (byte-array length)]
      (.read bis buffer 0 length)
      buffer)))

(defn create-vm []
  {:display {:width 64
             :height 32
             :pixels (vec (boolean-array (* 64 32)))}
   ;; memory
   ;;     0 - 0x200 : interpreter or font data in modern chip 8s
   ;; 0xea0 - 0xeff : call stack
   ;; 0xf00 - 0xfff : display refresh
   :memory (vec (byte-array 0x1000))
   :registers (vec (byte-array 16))
   :address-register (byte 0)
   ;; timers run at 60 hertz
   :timers {:delay 0
            :sound 0}
   :keyboard (vec (boolean-array 16))})

(defn- set-memory [vm index byte]
  (assoc-in vm [:memory index] byte))

(defn load-program [vm f]
  (let [bytes (vec (file->bytes f))
        len (count bytes)]
    (assoc vm
      :memory (vec (concat bytes
                           (drop len (:memory vm)))))))
