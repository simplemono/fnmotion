(ns fnmotion.html
  "A frame as HTML. The player hands a frame to Replicant in the browser;
  a render hands it to a headless Chrome as a string, without the keys
  only Replicant knows: `:on` and `:replicant/*`."
  (:require [clojure.walk :as walk]
            [hiccup2.core :as hiccup]))

(defn- replicant-key?
  [k]
  (or (= :on k)
      (and (keyword? k) (= "replicant" (namespace k)))))

(defn plain
  "The hiccup without what only Replicant understands."
  [hiccup]
  (walk/postwalk (fn [x]
                   (if (and (vector? x) (keyword? (first x)) (map? (second x)))
                     (update x 1 #(into {} (remove (comp replicant-key? key)) %))
                     x))
                 hiccup))

(defn html
  "The hiccup as an HTML string."
  [hiccup]
  (str (hiccup/html (plain hiccup))))

(comment
  (html [:div {:style {:opacity 0.5} :on {:click []}} (list [:b "x"] nil)])
  )
