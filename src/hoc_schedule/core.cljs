(ns hoc-schedule.core
  (:require [goog.dom :as gdom]
            [poor.hiccup :refer [h]]
            [goog.string]
            [goog.string.format]
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
  (for [{:strs [id name start_time end_time]} (apply concat (vals activities))]
    {:name name
     :url (str "https://activities.heartofclojure.eu/activities/" id)
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
  (get ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"]
       (.getDay dt)))

(defn format-time [ts]
  (let [dt (date-time ts)]
    (goog.string.format "%02d:%02d" (.getHours dt) (.getMinutes dt))))

(defn event-view [day-events]
  [:table.collapse.ba.br2.b--black-10.pv2.ph3.mt4.sans-serif
   [:thead
    [:tr.tac
     [:th "Time"]
     [:th "Event / Activity"]]]
   [:tbody
    (for [[_ events] day-events
          :let [dt (date-time (:start (first events)))]]
      (cons
       [:tr.striped--near-white
        [:td.ph3.pt3.pb2.b {:colspan 2} (day-of-week dt) " " (day-of-month dt) "/" (month dt)]]
       (for [{:keys [name start end type url]} events]
         [:tr.striped--near-white
          [:td.pv2.ph3 (format-time start) " — " (format-time end)]
          [(if (= :activity type) :td.pv2.ph3.bg-washed-green :td.pv2.ph3)
           (if url
             [:a {:href url} name]
             name)]])))]])

(defn draw [activities schedule]
  (def activities activities)
  (def schedule schedule)
  (gdom/removeChildren (app-element))
  (let [day-events (->> (activities->events activities)
                        (concat (schedule->events schedule))
                        (group-by (comp (juxt month day-of-month) date-time :start))
                        (into {} (map (juxt key (comp (partial sort-by :start) val))))
                        (sort-by key))]
    (gdom/append (app-element) (h (event-view day-events)))))

(defn fetch-data! []
  (.then
   (js/Promise.all
    ;; These are updated every two minutes from the clashfinder/activities app
    ;; We can't fetch directly from the source because of CORS restrictions.
    #js [(js/fetch "https://arnebrasseur.net/hoc/activities.json")
         (js/fetch "https://arnebrasseur.net/hoc/schedule.json")]
    #_#js [(js/fetch "activities.json") (js/fetch "schedule.json")]
    )
   (fn [resp]
     (.then
      (js/Promise.all (map #(.json %) resp))
      (fn [vals]
        (apply draw (map js->clj vals)))))))

(fetch-data!)
