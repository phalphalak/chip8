(ns chip8.core
  (:require [clojure.java.io :refer [file input-stream]]
            [clojure.string :as str]))

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

(defn file->bytes [f]
  (with-open [bis (input-stream f)]
    (let [length (.length (file f))
          buffer (byte-array length)]
      (.read bis buffer 0 length)
      buffer)))


(defn reset [vm]
  ; memory to zero
  )

(def ^:private hex-chars (mapv unchecked-byte
                               [0xF0, 0x90, 0x90, 0x90, 0xF0, ;; 0
                                0x20, 0x60, 0x20, 0x20, 0x70, ;; 1
                                0xF0, 0x10, 0xF0, 0x80, 0xF0, ;; 2
                                0xF0, 0x10, 0xF0, 0x10, 0xF0, ;; 3
                                0x90, 0x90, 0xF0, 0x10, 0x10, ;; 4
                                0xF0, 0x80, 0xF0, 0x10, 0xF0, ;; 5
                                0xF0, 0x80, 0xF0, 0x90, 0xF0, ;; 6
                                0xF0, 0x10, 0x20, 0x40, 0x40, ;; 7
                                0xF0, 0x90, 0xF0, 0x90, 0xF0, ;; 8
                                0xF0, 0x90, 0xF0, 0x10, 0xF0, ;; 9
                                0xF0, 0x90, 0xF0, 0x90, 0x90, ;; A
                                0xE0, 0x90, 0xE0, 0x90, 0xE0, ;; B
                                0xF0, 0x80, 0x80, 0x80, 0xF0, ;; C
                                0xE0, 0x90, 0x90, 0x90, 0xE0, ;; D
                                0xF0, 0x80, 0xF0, 0x80, 0xF0, ;; E
                                0xF0, 0x80, 0xF0, 0x80, 0x80  ;; F
                                ]))

(defn- set-memory [vm index byte]
  (assoc-in vm [:memory index] byte))

(defn memory [vm index]
  {:pre [(pos? index) (< index (count (:memory vm)))]}
  (bit-and 0xff (get-in vm [:memory index])))

(defn create-vm []
  (let [vm {:display {:width 64
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
            :keyboard (vec (boolean-array 16))}]
    (reduce (fn [vm [i val]]
              (set-memory vm i val))
            vm
            (map-indexed vector hex-chars))))

(defn pc [vm]
  (:pc vm))

(defn jump [vm addr]
  (assoc vm :pc addr))

(defn opcode [vm]
  (+ (bit-shift-left (memory vm (pc vm)) 8)
     (memory vm (inc (pc vm)))))

(defn increment-pc [vm]
  (update-in vm [:pc] + 2))

(defn error
  ([vm]
   (:error vm))
  ([vm message]
   (assoc vm :error message)))

(defn- byte->long [o]
  (bit-and 0xff o))

(defn- byte? [o]
  (instance? Byte o))

(defn- long? [o]
  (instance? Long o))

(defn- register [vm i]
  {:pre [(long? i)]}
  (get-in vm [:registers i]))

(defn- set-register [vm i byte]
  {:pre [(byte? byte)]}
  (assoc-in vm [:registers i] byte))

(defn- random-byte [mask]
  (let [mask (if (byte? mask)
               mask
               (unchecked-byte mask))
        bytes (byte-array 1)]
    (.nextBytes (java.util.Random.) bytes)
    (byte (bit-and (first bytes) mask))))

(defn- add-bytes [b1 b2]
  {:pre [(byte? b1) (byte? b2)]
   :post (byte? (first %))}
  (let [r (+ b1 b2)
        c (<= Byte/MIN_VALUE r Byte/MAX_VALUE)]
    [(unchecked-byte r) c]))

(defn process-opcode [vm opcode]
  (prn (Integer/toString opcode 16))
  (op-case
   opcode
   ;; JP addr
   :1nnn (jump vm nnn)
   ;; SE Vx, byte
   :3xkk (-> (if (= (register vm x)
                    (unchecked-byte kk))
               (increment-pc vm)
               vm)
             increment-pc)
   ;; ADD Vx, byte
   :7xkk (let [[val _] (add-bytes (register vm x) (unchecked-byte kk))]
           (-> vm
               (set-register x val)
               increment-pc))
   ;; LD I, addr
   :annn (-> (assoc vm :address-register nnn)
             increment-pc)
   ;; RND Vx, byte
   :cxkk (-> (set-register vm x (random-byte kk))
             increment-pc)
   ;; DRW Vx, Vy, nibble
   :dxyn (do (prn (format "pretend to draw %s-byte sprite at (V%s,V%s)" n x y))
             ;; todo set VF to 1 if collision, 0 otherwise
             (increment-pc vm))
   ;; else
   (error vm (format "unkown opcode 0x%x" opcode))))

(defn step [vm]
  (process-opcode vm (opcode vm)))

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
