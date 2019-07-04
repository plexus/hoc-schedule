(ns hoc-schedule.core
  (:require [goog.dom :as gdom]
            [poor.hiccup :refer [h]]
            [goog.string :refer [format]]
            [goog.date.DateTime :as DateTime])
  (:import [goog.date DateTime]))

(defn app-element []
  (gdom/getElement "hoc-schedule"))

(defn schedule->events [schedule]
  (for [{:strs [name start end]} (get-in schedule ["locations" 0 "events"])]
    {:name name
     :start (js/Date.parse start)
     :end (js/Date.parse end)
     :type :schedule}))

(defn activities->events [activities]
  (for [{:strs [name start_time end_time]} (apply concat (vals activities))]
    {:name name
     :start (js/Date.parse start_time)
     :end (js/Date.parse end_time)
     :type :activity}))

(defn date-time [ts]
  (DateTime/fromTimestamp ts))

(defn month [dt]
  (inc (.getMonth dt)))

(defn day-of-month [dt]
  (.getDate dt))

(defn day-of-week [dt]
  (get ["Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"]
       (.getDay dt)))

(defn format-time [ts]
  (let [dt (date-time ts)]
    (format "%02d:%02d" (.getHours dt) (.getMinutes dt))))

(defn event-view [day-events]
  [:table
   [:thead
    [:tr.tac
     [:th "Time"]
     [:th "Event / Activity"]]]
   [:tbody
    (apply concat
           (for [[_ events] day-events
                 :let [dt (date-time (:start (first events)))]]
             (cons
              [:tr
               [:td (day-of-week dt) " " (day-of-month dt) "/" (month dt)]]
              (for [{:keys [name start end type]} events]
                [:tr
                 [:td (format-time start) " — " (format-time end)]
                 [(if (= :activity type)
                    :td.bg-washed-green
                    :td)
                  name]]))))]])

(defn draw [activities schedule]
  (def activities activities)
  (def schedule schedule)
  (gdom/removeChildren (app-element))
  (let [day-events (->> (activities->events activities)
                        (concat (schedule->events schedule))
                        (group-by (comp (juxt month day-of-month) date-time :start))
                        (sort-by key))]
    (gdom/append (app-element) (h (event-view day-events)))))

(defn fetch-data! []
  (.then
   (js/Promise.all
    #js[(js/fetch "activities.json")
        (js/fetch "schedule.json")])
   (fn [resp]
     (.then
      (js/Promise.all (map #(.json %) resp))
      (fn [vals]
        (apply draw (map js->clj vals)))))))

(fetch-data!)
