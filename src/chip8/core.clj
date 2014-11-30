(ns chip8.core
  (:require [clojure.java.io :refer [file input-stream]]
            [clojure.string :as str]
            [clojure.core.match :refer [match]])
;  (:import [java.io File])
  )

(defn file->bytes [f]
  (with-open [bis (input-stream f)]
    (let [length (.length (file f))
          buffer (byte-array length)]
      (.read bis buffer 0 length)
      buffer)))


(defn reset [vm]
  ; memory to zero
  )

(defn create-vm []
  {:display {:width 64
             :height 32
             :pixels (vec (boolean-array (* 64 32)))}
   :error nil
   :clock-speed 60 ;; hertz
   :pc 0x200
   ;; memory
   ;;     0 - 0x200 : interpreter or font data in modern chip 8s
   ;; 0xea0 - 0xeff : call stack
   ;; 0xf00 - 0xfff : display refresh
   :memory (vec (byte-array 0x1000))
   :registers (vec (byte-array 16)) ;; a.k.a. V0 to VF (last one acts as a carry over)
   :address-register (byte 0) ;; a.k.a. I
   ;; timers run at 60 hertz
   :timers {:delay 0
            :sound 0}
   :keyboard (vec (boolean-array 16))})

#_(defn- set-memory [vm index byte]
  (assoc-in vm [:memory index] byte))

(defn memory [vm index]
  {:pre [(pos? index) (< index (count (:memory vm)))]}
  (bit-and 0xff (get-in vm [:memory index])))

(defn pc [vm]
  (:pc vm))

(defn opcode [vm]
  (map (partial memory vm)
       (range (pc vm)
              (+ 4 (pc vm)))))

(defn increment-pc [vm]
  (update-in vm [:pc] + 4))

(defn process-opcode [vm opcode]
  :todo
  vm)

(defn op-matcher [hex val form]
  (let [mask (read-string
              (str "0x"
                   (-> (name hex)
                       (str/replace #"[0-9a-f]" "f")
                       (str/replace #"[^f]" "0"))))
        expected (read-string
                  (str "0x"
                       (-> (name hex)
                           (str/replace #"[^0-9a-f]" "0"))))
        binding-mask (-> (name hex)
                         (str/replace #"[0-9a-f]" "_"))
        binding (reduce (fn [acc group]
                          (let [sym (symbol (apply str (map first group)))
                                mask' (apply + (map second group))
                                trailing-zeros (Integer/numberOfTrailingZeros mask')
                                masked (list 'bit-and mask' val)]
                            (if (= (symbol "_") sym)
                              acc
                              (conj acc
                                    (if (zero? trailing-zeros)
                                      masked
                                      (list 'bit-shift-right
                                            masked
                                            trailing-zeros))
                                    sym))))
                        '()
                        (partition-by first (map (fn [c m] [c m])
                                                 binding-mask
                                                 [0xf000 0x0f00 0x00f0 0x000f])))]
    (list
     (list '= (list 'bit-and mask val) expected)
     (list 'let (vec binding) form))))

(defmacro op-case [op-var# & forms]
  (let [else? (odd? (count forms))]
    (loop [result (list 'cond)
           forms' (partition 2 forms)]
      (if (seq forms')
        (let [[[hex form] & rest] forms']
          (recur (concat result (op-matcher hex op-var# form)) rest))
        (if else?
          (concat result (list :else (last forms)))
          result)))))


(defn step [vm]
  (-> vm
      (process-opcode (opcode vm))
      increment-pc))

(defn load-program [vm f]
  (let [bytes (vec (file->bytes f))
        len (count bytes)
        mem (:memory vm)]
    (assoc vm
      :memory (vec (concat (take 0x200 mem)
                           bytes
                           (drop (+ 0x200 len) mem))))))


(comment
  (load-program (create-vm) "resources/Maze [David Winter, 199x].ch8"))
