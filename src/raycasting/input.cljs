(ns raycasting.input)

(defonce key-states (atom {}))
(defonce focus false)

(defn valid-key? [key]
  (or (= key "Escape")
      (= key "Shift")
      (= key "ArrowLeft")
      (= key "ArrowUp")
      (= key "ArrowRight")
      (= key "ArrowDown")))

(defn on-key-press [event]
  (when focus
    (let [key (. event -key)]
      (when (valid-key? key)
        (swap! key-states assoc key true)))))

(defn on-key-release [event]
  (when focus
    (let [key (. event -key)]
      (when (valid-key? key)
        (swap! key-states assoc key false)))))

(defn on-click [event]
  (set! focus true))

(defn release-focus []
  (set! focus false))