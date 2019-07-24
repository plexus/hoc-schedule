(ns hoc-schedule.core
  (:require [goog.dom :as gdom]
            [poor.hiccup :refer [h]]
            [goog.string]
            [goog.string.format]
            [goog.date.DateTime :as DateTime])
  (:import [goog.date DateTime]))

(defn app-element []
  (gdom/getElement "hoc-schedule"))

(def activitiy-date-pattern #"(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2})")
(def schedule-date-pattern #"(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})")

(defn parse-int [s]
  (let [[_ i] (re-find #"0*(\d+)" s)]
    (js/parseInt s)))

(defn parse-date [pattern date-string]
  (let [[_ & parts] (re-find pattern date-string)
        [year month day hours minutes] (map parse-int parts)]
    (js/Date. year (dec month) day hours minutes)))

(defn schedule->events [schedule]
  (for [{:strs [name start end]} (get-in schedule ["locations" 0 "events"])]
    {:name name
     :start (parse-date schedule-date-pattern start)
     :end (parse-date schedule-date-pattern end)
     :type :schedule}))

(defn activities->events [activities]
  (for [{:strs [id name start_time end_time]} (apply concat (vals activities))]
    {:name name
     :url (str "https://activities.heartofclojure.eu/activities/" id)
     :start (parse-date activitiy-date-pattern start_time)
     :end (parse-date activitiy-date-pattern end_time)
     :type :activity}))

(defn month [dt]
  (inc (.getMonth dt)))

(defn day-of-month [dt]
  (.getDate dt))

(defn day-of-week [dt]
  (get ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"]
       (.getDay dt)))

(defn format-time [dt]
  (goog.string.format "%02d:%02d" (.getHours dt) (.getMinutes dt)))

(defn event-view [day-events]
  [:table.collapse.ba.br2.b--black-10.pv2.ph3.mt4.sans-serif
   [:thead
    [:tr.tac
     [:th "Time"]
     [:th "Event / Activity"]]]
   [:tbody
    (for [[_ events] day-events
          :let [dt (:start (first events))]]
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
                        (group-by (comp (juxt month day-of-month) :start))
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
