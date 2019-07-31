(ns hoc-schedule.core
  (:require [goog.dom :as gdom]
            [poor.hiccup :refer [h]]
            [goog.object :as gobj]
            [goog.string]
            [goog.string.format]
            [goog.date.DateTime :as DateTime])
  (:import [goog.date DateTime]))

(defn app-element []
  (gdom/getElement "hoc-schedule"))

(def activitiy-date-pattern #"(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2})")
(def schedule-date-pattern #"(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})")
(def clashfinder-id->speaker-id
  {"yulias(1)" "yulia-startsev"
  "fabriz(1)" "fabrizio-ferrai"
  "lisapa(1)" "lisa-passing"
  "chrisa(1)" "chris-adams"
  "lilymg(1)" "lily-m-goh"
  "tiagol(1)" "tiago-luchini"
  "meybei(1)" "mey-beisaron"
  "ramnhu(1)" "ramon-huidobro"
  "sarahg(1)" "sarah-groff-hennigh-palermo"
  "daniel1(1)" "daniel-compton"
  "chelse(1)" "chelsey-mitchell"
  "maarte(1)" "maarten-truyens"
  "jivago1(1)" "jivago-alves"
  "tobias(1)" "tobias-pfeiffer"
  "rachel(1)" "rachel-lawson"})

(defn parse-int [s]
  (let [[_ i] (re-find #"0*(\d+)" s)]
    (js/parseInt s)))

(defn parse-date [pattern date-string]
  (let [[_ & parts] (re-find pattern date-string)
        [year month day hours minutes] (map parse-int parts)]
    (js/Date. year (dec month) day hours minutes)))

(defn schedule->events [schedule]
  (let [all-speaker-data (js->clj (gobj/get js/window "hoc_speaker_data"))]
    (for [{:strs [name start end short] :as x} (get-in schedule ["locations" 0 "events"])
          :let [speaker (->> all-speaker-data
                             (filter #(= (get % "id") (clashfinder-id->speaker-id short)))
                             first)]]
      {:name name
       :speaker-data speaker
       :start (parse-date schedule-date-pattern start)
       :end (parse-date schedule-date-pattern end)
       :type (if speaker :talk :schedule)})))

(defn activities->events [activities]
  (for [{:strs [id name start_time end_time] :as a} (apply concat (vals activities))]
    {:name name
     :url (str "https://activities.heartofclojure.eu/activities/" id)
     :start (parse-date activitiy-date-pattern start_time)
     :end (parse-date activitiy-date-pattern end_time)
     :img (get a "image_url")
     :participants (get a "participations_count")
     :max-participants (get a "limit_of_participants")
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
  [:div
   (for [[_ events] day-events
         :let [dt (:start (first events))]]
     [:div
      [:div.f2.tc.pa3.mt5 (day-of-week dt) " " (day-of-month dt) "/" (month dt)]
      (for [{:keys [name start end type url] :as ev} events]
        (if (= "current-time" type)
          [:div.bg-pink-t60.f3.tc.white.b.pa2 {:id "current-time"} (str "👉  It's " (day-of-week dt) "  👈")]
          [:div.dt.ba.b--near-white.ph2.pv3.pa3-ns.w-100
           [:div.dtc.w3.h3.w4-ns.h4-ns
            (case type
              :activity [:img.w3.h3.w4-ns.h4-ns.br-100 {:src (:img ev)}]
              :talk [:img.w3.w4-ns {:src (str "https://heartofclojure.eu" (get-in ev [:speaker-data "img"]))}]
              [:div])]
           [:div.v-mid.dtc.ph3
            [:div.mb2
             [:span.dib.v-mid.gray.f4 (format-time start) " — " (format-time end)]
             (case type
               :activity [:span.dib.v-mid.ttu.bg-green.pv1.ph2.br2.b.white.ml2.f7.o-60 "activity"]
               :talk [:span.dib.v-mid.ttu.bg-pink-t60.pv1.ph2.br2.b.white.ml2.f7.o-60
                      (if (get-in ev [:speaker-data "keynote"]) "keynote" "talk")]
               nil)]
            (case type
              :activity [:a.b.db.f3.link.pink-t60
                         {:href url
                          :target "_blank"}
                         name]
              :talk [:a.b.db.f3.link.pink-t60
                     {:href (str "https://heartofclojure.eu/speakers#" (get-in ev [:speaker-data "id"]))
                      :target "_blank"}
                     name]
              [:span.b.db.f3 name])
            (when (= :activity type)
              (let [remaining (- (:max-participants ev) (:participants ev))]
                [:span.db.mt2
                 (if (pos? remaining)
                   (str remaining " / " (:max-participants ev) " spots available")
                   "fully booked")]))]]))])])

(defn draw [activities schedule]
  (def activities activities)
  (def schedule schedule)
  (gdom/removeChildren (app-element))
  (let [day-events (->> (activities->events activities)
                        (into (schedule->events schedule))
                        (into [{:start (js/Date.) :end (js/Date.) :type "current-time"}])
                        (group-by (comp (juxt month day-of-month) :start))
                        (into {} (map (juxt key (comp (partial sort-by :end) val))))
                        (sort-by key))]
    (gdom/append (app-element) (h (event-view day-events)))
    (set! js/window.location.href "#current-time")))

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
