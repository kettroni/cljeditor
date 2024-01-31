(ns cljeditor.core
  (:require
   [lanterna.screen :as s]))

(defrecord ModeLine [text color-options])
(defrecord Mode [keymap modeline])
(defrecord BufferState [screen mode buffer])

(defn- create-modeline-putter [screen]
  (let [[_ screen-length] (s/get-size screen)
        modeline-row (dec screen-length)]
    (partial s/put-string screen 0 modeline-row)))

(defn- move-cursor-forward
  ([screen]
   (let [[x y] (s/get-cursor screen)]
     (s/move-cursor screen (inc x) y)))
  ([screen [x y]]
   (s/move-cursor screen (inc x) y)))

(defn- move-cursor-down
  ([screen]
   (let [[x y] (s/get-cursor screen)]
     (s/move-cursor screen x (inc y))))
  ([screen [x y]]
   (s/move-cursor screen x (inc y))))

(defn- move-cursor-backward
  ([screen]
   (let [[x y] (s/get-cursor screen)]
     (s/move-cursor screen (dec x) y)))
  ([screen [x y]]
   (s/move-cursor screen (dec x) y)))

(defn- move-cursor-up
  ([screen]
   (let [[x y] (s/get-cursor screen)]
     (s/move-cursor screen x (dec y))))
  ([screen [x y]]
   (s/move-cursor screen x (dec y))))

(defn- put-string-on-position [screen [x y] string]
  (s/put-string screen x y string))

(defn- insert-char-and-move-cursor [screen key]
  (let [cursor-position (s/get-cursor screen)]
    (put-string-on-position screen cursor-position (str key))
    (move-cursor-forward screen cursor-position)))

(defn- delete-char-before-cursor [screen]
  (let [[x y] (s/get-cursor screen)]
    (put-string-on-position screen [(dec x) y] " ")
    (move-cursor-backward screen [x y])))

(def ^:private insert-mode
  (->Mode
   {:escape (fn [state _]
              (move-cursor-backward (:screen state))
              :normal-mode)
    :backspace (fn [state _] (delete-char-before-cursor (:screen state)))
    :default-command (fn [state key]
                   (insert-char-and-move-cursor (:screen state) key))}
   (->ModeLine "*insert mode*" {:fg :black :bg :red})))

(def ^:private normal-mode
  (->Mode
   {\a (fn [state _]
         (move-cursor-forward (:screen state))
         :insert-mode)
    \i (fn [_ _] :insert-mode)
    \q (fn [state _]
         (s/stop (:screen state))
         (map->BufferState nil))
    \h (fn [state _] (move-cursor-backward (:screen state)))
    \l (fn [state _] (move-cursor-forward (:screen state)))
    \j (fn [state _] (move-cursor-down (:screen state)))
    \k (fn [state _] (move-cursor-up (:screen state)))
    :default-command (fn [_ _] nil)}
   (->ModeLine "*normal mode*" {:fg :black :bg :green})))

(def ^:private modes
  {:normal-mode normal-mode
   :insert-mode insert-mode})

(defn- get-new-state
  [state]
  (let [screen (:screen state)
        key (s/get-key-blocking screen)
        command (get-in state [:mode :keymap key] (get-in state [:mode :keymap :default-command]))
        result (command state key)
        mode (get modes result)]
    (if mode
      (assoc state :mode mode)
      (do (s/redraw screen)
          (recur state)))))

(defn- put-modeline [new-screen modeline]
  (let [modeline-putter (create-modeline-putter new-screen)]
    (modeline-putter (:text modeline) (:color-options modeline))))

(defn- get-modeline [state]
  (get-in state [:mode :modeline]))

(defn- main-loop [state]
  (let [new-state (get-new-state state)
        new-screen (:screen new-state)]
    (when new-screen
      (put-modeline new-screen (get-modeline new-state))
      (s/redraw new-screen)
      (recur new-state))))

(defn- create-initial-state []
  (let [initial-screen (s/get-screen :swing)]
    (s/start initial-screen)
    (put-modeline initial-screen (:modeline normal-mode))
    (s/redraw initial-screen)
    (->BufferState initial-screen normal-mode [])))

(defn -main [& _]
  (main-loop (create-initial-state)))

(comment
  (-main)

  (def x (byte-array 10 (byte 0)))

  (byte \n)
  (count (String. x))

  (defn allocate-buffer
    ([[col row]]
     (to-array-2d (repeat row (byte-array col)))))

  (defn set-byte
    [buffer [x y] char]
    (aset buffer x y char))

  (defn get-byte
    [buffer [x y]]
    (aget buffer x y))

  (defn get-row-bytestring
    [buffer row]
    (vec (aget buffer row)))

  (defn get-row-characters
    [buffer row]
    (map char (get-row-bytestring buffer row)))

  (def my-buffer (allocate-buffer [10 5]))
  (map vec my-buffer)
  (get-row-bytestring my-buffer 4)
  (get-row-characters my-buffer 4)
  (get-byte my-buffer [2 5])
  (get-byte my-buffer [4 5])
  (set-byte my-buffer [4 5] (byte 11))

  (map (fn [x] (vec x)) my-buffer)

  (def bs (byte-array 10 (byte 0)))
  (vec bs)
  (String. bs)

  (aset-byte bs)

  (def scr
    (s/get-screen :swing))

  (s/start scr)
  (s/get-size scr)

  (s/clear scr)
  (s/stop scr)

  (s/in-screen scr))
