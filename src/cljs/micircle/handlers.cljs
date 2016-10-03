(ns micircle.handlers
  ;(:require-macros [com.rpl.specter :refer [select]])
  (:require [re-frame.core :as re-frame :refer [reg-event-db reg-event-fx]]
            [micircle.db :as db]
            [day8.re-frame.http-fx]
            [ajax.core :as ajax :refer [GET POST]]
            [micircle.math :as math]
            [com.rpl.specter :as s]))

(defn identifiers [data]
  (s/select [:data s/ALL #(= "interactor" (:object %)) (s/collect-one :identifier) :id] data))

(defn participants [data]
  (first (s/select [:data s/ALL #(= "interaction" (:object %)) :participants] data)))



(re-frame/reg-event-db
  :initialize-db
  (fn [_ _]
    db/default-db))

(re-frame/reg-event-db
  :set-active-panel
  (fn [db [_ active-panel]]
    (assoc db :active-panel active-panel)))

(defn degree-scale [total amount]
  (* 360 (/ amount total)))

(reg-event-fx
  :shape-entities
  (fn [{db :db}]
    (let [scale-fn (partial degree-scale (apply + (map (fn [p] (get-in db [:lengths (:interactorRef p)])) (participants (:data db)))))]
      {:db       (assoc-in db [:views :entities]
                           (reduce (fn [v p]
                                     (let [p-length (get-in db [:lengths (:interactorRef p)])
                                           total    (apply + (map :length (vals v)))]
                                       (assoc v (keyword (:id p)) {:interactorRef (keyword (:interactorRef p))
                                                                   :id            (keyword (:id p))
                                                                   :length        p-length
                                                                   :start-angle   (scale-fn total)
                                                                   :end-angle     (- (scale-fn (+ total p-length)) 4)})))
                                   {}
                                   (participants (:data db))))
       :dispatch [:shape-links]})))

(defn parse-pos [str]
  (map (fn [part] (let [parsed (js/parseInt part)] (if-not (js/isNaN parsed) parsed nil))) (clojure.string/split str "-")))

(defn radial-scale [[lower-domain upper-domain] [lower-range upper-range]]
  (fn [input]
    (let [percent (/ input (- upper-domain lower-domain))]
      (+ lower-range (* percent (- upper-range lower-range))))))

(reg-event-db
  :shape-links
  (fn [db]
    (let [entity-views (get-in db [:views :entities])
          feature-map  (reduce (fn [total [participant-id next]]
                                 (assoc total (keyword (:id next)) (assoc next :participant-id (keyword participant-id))))
                               {}
                               (s/select [s/ALL (s/collect-one :id) :features s/ALL] (participants (:data db))))]
      (let [links (map (fn [[id {:keys [participant-id linkedFeatures sequenceData] :as details}]]
                         (let [[from-pos-1 from-pos-2] (parse-pos (:pos (first sequenceData)))
                               [to-pos-1 to-pos-2] (parse-pos (:pos (first (get-in feature-map [(-> linkedFeatures first keyword) :sequenceData]))))
                               from-participant-view (participant-id entity-views)
                               to-participant-view   (get entity-views (get-in feature-map [(-> linkedFeatures first keyword) :participant-id]))]

                           {:start-angle-1 ((radial-scale
                                              [0 (:length from-participant-view)]
                                              [(:start-angle from-participant-view) (:end-angle from-participant-view)])
                                             from-pos-1)
                            :start-angle-2 ((radial-scale
                                              [0 (:length from-participant-view)]
                                              [(:start-angle from-participant-view) (:end-angle from-participant-view)])
                                             from-pos-2)
                            :end-angle-1   ((radial-scale
                                              [0 (:length to-participant-view)]
                                              [(:start-angle to-participant-view) (:end-angle to-participant-view)])
                                             to-pos-1)
                            :end-angle-2   ((radial-scale
                                              [0 (:length to-participant-view)]
                                              [(:start-angle to-participant-view) (:end-angle to-participant-view)])
                                             to-pos-2)}))
                       feature-map)]
        (assoc db :feature-map feature-map
                  :link-views links)))))

(reg-event-fx
  :success-fetch-complex
  (fn [{db :db} [_ response]]
    {:db         (assoc db :data response)
     :dispatch-n (map (fn [[{id :id} label]]
                        [:fetch-length id label]) (identifiers response))}))

(reg-event-fx
  :success-fetch-length
  (fn [{db :db} [_ id label [{length :length}]]]
    (let [new-db (assoc-in db [:lengths label] (js/parseInt length))]
      (cond->
        {:db new-db}
        (>= (count (keys (get-in new-db [:lengths]))) 3) (merge {:dispatch [:shape-entities]})))))

(reg-event-fx
  :fetch-length
  (fn [{:keys [db]} [_ id label]]
    {:db         (assoc db :show-twirly true)
     :http-xhrio {:method          :get
                  :uri             (str "http://www.uniprot.org/uniprot/?format=json&columns=length,id&query=accession:" id)
                  :timeout         30000
                  :response-format (ajax/json-response-format {:keywords? true})
                  :on-success      [:success-fetch-length id label]
                  :on-failure      [:bad-http-result]}}))

(reg-event-fx
  :fetch-complex
  (fn [{:keys [db]} [_ id]]
    {:db         (assoc db :show-twirly true)
     :http-xhrio {:method          :get
                  :uri             (str "http://www.ebi.ac.uk/intact/complex-ws/export/" id)
                  :timeout         30000
                  :response-format (ajax/json-response-format {:keywords? true})
                  :on-success      [:success-fetch-complex]
                  :on-failure      [:bad-http-result]}}))
