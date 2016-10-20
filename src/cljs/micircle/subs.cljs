(ns micircle.subs
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :as re-frame :refer [reg-sub]]
            [com.rpl.specter :as s]))

(re-frame/reg-sub
  :name
  (fn [db]
    (:name db)))

(re-frame/reg-sub
  :active-panel
  (fn [db _]
    (:active-panel db)))

(reg-sub
  :proteins
  (fn [db]
    (get-in db [:lengths])))

(reg-sub
  :app-db
  (fn [db] db))

(reg-sub
  :link-views
  (fn [db]
    (filter (fn [{:keys [start-angle-1 end-angle-1]}]
              (> start-angle-1 end-angle-1)) (:link-views db))))

(reg-sub
  :participants
  (fn [db]
    (first (s/select [:data
                      :data
                      s/ALL
                      #(= "interaction" (:object %))
                      :participants]
                     db))))

(reg-sub
  :views
  (fn [db]
    (:participants (:views db))))

(reg-sub
  :features
  (fn [db]
    (:features (:views db))))

(reg-sub
  :options
  (fn [db]
    (:options db)))

(reg-sub
  :flags
  (fn [db]
    (:flags db)))

(reg-sub
  :complex-id
  (fn [db]
    (:complex-id db)))

(reg-sub
  :superfamily-features
  (fn [db]
    (get-in db [:views :superfamily])))

(reg-sub
  :participant-views
  (fn [db]
    (get-in db [:views :participants])))



(reg-sub
  :link
  (fn [db [_ from-id to-id]]
    ; The JSON returns string values. If you can't beat them, join them.
    (let [[from-id to-id] (map str [from-id to-id])
          from-participant (get-in db [:views :participants from-id])
          to-participant   (get-in db [:views :participants from-id])]
      from-participant)))

(defn radial-scale [[lower-domain upper-domain] [lower-range upper-range]]
  (fn [input]
    (let [percent (/ input (- upper-domain lower-domain))]
      (+ lower-range (* percent (- upper-range lower-range))))))

(reg-sub
  :feature
  (fn [db]
    1))

(reg-sub
  :feature-map
  (fn [db]
    (get db :feature-map)))

(reg-sub
  :all-features
  (fn [db]
    (get db :features)))

