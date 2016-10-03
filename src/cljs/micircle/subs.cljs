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
    (vals (:entities (:views db)))))

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

