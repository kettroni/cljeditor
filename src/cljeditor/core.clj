(ns cljeditor.core
  (:require
   [lanterna.screen :as s]))

(defrecord EditorState [screen mode])

(defn- create-modeline-putter [screen]
  (let [[_ screen-length] (s/get-size screen)
        modeline-row (dec screen-length)]
    (partial s/put-string screen 0 modeline-row)))

(defn- render-modeline [screen mode]
  (let [put-msg-to-modeline (create-modeline-putter screen)]
    (case mode
      \n (put-msg-to-modeline "*normal mode*" {:fg :black :bg :green})
      \i (put-msg-to-modeline "*insert mode*" {:fg :black :bg :red}))
    (s/redraw screen)))

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

(defn- create-editor-state-in-mode [screen mode]
  (render-modeline screen mode)
  (->EditorState screen mode))

(defn- handle-normal-mode-key [screen key]
  (case key
    \a (do (move-cursor-forward screen)
           (create-editor-state-in-mode screen \i))
    \i (create-editor-state-in-mode screen \i)
    \q (s/stop screen) ;; TODO: figure out if the return nil here is a problem
    \h (move-cursor-backward screen)
    \l (move-cursor-forward screen)
    \j (move-cursor-down screen)
    \k (move-cursor-up screen)
    nil))

(defn- mode-loop [screen handle-x-mode-key-fn]
  (let [key (s/get-key-blocking screen)]
    (or (handle-x-mode-key-fn screen key)
        (do (s/redraw screen)
            (recur screen handle-x-mode-key-fn)))))

(defn- insert-char-and-move-cursor [screen key]
  (let [cursor-position (s/get-cursor screen)]
    (put-string-on-position screen cursor-position (str key))
    (move-cursor-forward screen cursor-position)))

(defn- delete-char-before-cursor [screen]
  (let [[x y] (s/get-cursor screen)]
    (put-string-on-position screen [(dec x) y] " ")
    (move-cursor-backward screen [x y])))

(defn- handle-insert-mode-key [screen key]
  (case key
    :escape (do (move-cursor-backward screen)
                (create-editor-state-in-mode screen \n))
    :backspace (delete-char-before-cursor screen)
    (insert-char-and-move-cursor screen key)))

(defn- main-loop [state]
  (let [screen (:screen state)
        new-state (case (:mode state)
                 \n (mode-loop screen handle-normal-mode-key)
                 \i (mode-loop screen handle-insert-mode-key))]
    (when new-state
      (recur new-state))))

(defn- create-initial-state []
  (let [initial-screen (s/get-screen :swing)]
    (s/start initial-screen)
    (create-editor-state-in-mode initial-screen \n)))

(defn -main [& _]
  (main-loop (create-initial-state)))

(comment
  (-main))

;; Screen playground.
(comment
  (def scr (s/get-screen :swing))
  (s/start scr)
  (s/put-string scr 0 0 "Hello, World!")
  (s/put-string scr (->> (s/get-size scr)
                         first) 0 "Hello, You!")

  (s/clear scr)
  (s/redraw scr)

  ;; color example
  (s/put-string scr 0 12 "Red" {:fg :red})
  (s/put-string scr 0 13 "Green" {:fg :green})
  (s/put-string scr 0 14 "Yellow" {:fg :black :bg :yellow})
  (s/redraw scr)

  ;; move cursor
  (s/move-cursor scr 5 5)
  (s/move-cursor scr 0 0)

  ;; get cursor
  (s/get-cursor scr)

  )

;; Terminal playground.
(comment
  ;; "https://multimud.github.io/clojure-lanterna/"
  (require '[lanterna.terminal :as t])
  ;; Resize handler for the terminal.
  (def terminal-size (ref [0 0]))
  (defn handle-resize [cols rows]
    (dosync (ref-set terminal-size [cols rows])))

  (def term (t/get-terminal :swing {:resize-listener handle-resize}))

  ;; start terminal
  (t/start term)

  ;; utils
  (def put-character-to-term (partial t/put-character term))
  (def write #(dorun (map put-character-to-term %)))

  ;; writing
  (write "My name is Steve!")

  ;; moving cursor
  (t/move-cursor term 40 12)
  (write "@")
  (t/move-cursor term 0 0)

  ;; input
  (t/get-key term)

  ;; waits for next input.
  (t/get-key-blocking term)

  ;; waits with timeout
  (t/get-key-blocking term {:interval 100 :timeout 2000})

  ;; all special keys
  (require '[lanterna.constants :as c])
  (vals c/key-codes)

  ;; size of terminal
  (t/get-size term)
  @terminal-size

  ;; resetting
  (t/clear term)
  (t/stop term))
