(ns poor.hiccup
  (:require [goog.dom :as gdom]
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

(declare h)

(defn h* [hiccup]
  (let [els (h hiccup)]
    (if (seq? els)
      els
      (list els))))

(defn h [hiccup]
  (cond
    (string? hiccup)
    (gdom/createTextNode hiccup)

    (vector? hiccup)
    (let [[tag attrs children] (split-el hiccup)
          el (gdom/createElement tag)]
      (gdom/setProperties el (clj->js attrs))
      (apply gdom/append el (mapcat h* children))
      el)

    (seq? hiccup)
    (mapcat h* hiccup)

    :else
    (h (str hiccup))))
