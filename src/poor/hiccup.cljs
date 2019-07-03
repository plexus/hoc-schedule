(ns poor.hiccup
  (:require [goog.dom :as gdom]
            [cljs-bean.core :refer [bean ->clj ->js]]
            [clojure.string :as str]))

(defn split-tag [tag]
  (let [tag (name tag)
        parts (str/split tag #"\.")]
    [(first parts)
     (rest parts)]))

(defn split-el [[tag & tail]]
  (let [[tag kls] (split-tag tag)]
    [tag
     (cond-> (if (map? (first tail))
               (first tail)
               {})
       (seq kls)
       (update :class str (str/join " " kls)))
     (if (map? (first tail))
       (next tail)
       tail)]))

(defn h [hiccup]
  (cond
    (string? hiccup)
    (gdom/createTextNode hiccup)

    (vector? hiccup)
    (let [[tag attrs children] (split-el hiccup)
          el (gdom/createElement tag)]
      (gdom/setProperties el (->js attrs))
      (apply gdom/append el (mapcat (fn [child]
                                      (if (seq? child)
                                        (map h child)
                                        [(h child)]))
                                    children))
      el)

    :else
    (h (str hiccup))))
