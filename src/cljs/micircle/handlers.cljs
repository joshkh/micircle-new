(ns micircle.handlers
  ;(:require-macros [com.rpl.specter :refer [select]])
  (:require [re-frame.core :as re-frame :refer [reg-event-db reg-event-fx]]
            [micircle.db :as db]
            [day8.re-frame.http-fx]
            [ajax.core :as ajax :refer [GET POST]]
            [micircle.math :as math]
            [com.rpl.specter :as s]
            [tubax.core :refer [xml->clj]]))

(defn vecmap->map [key v]
  (reduce (fn [total next]
            (assoc total (get-in next key) next)) {} v))

(defn identifiers [data]
  (s/select [:data s/ALL #(= "interactor" (:object %)) (s/collect-one :identifier) :id] data))

(defn participants [data]
  (first (s/select [:data s/ALL #(= "interaction" (:object %)) :participants] data)))

(defn interactors [data]
  (s/select [:data s/ALL #(= "interaction")] data))

(defn classes [data]
  (s/select [:data s/ALL #(= "interactor")] data))



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
    (let [interactors (reduce (fn [total next] (assoc total (:id next) next)) {} (interactors (:data db)))
          scale-fn    (partial degree-scale (apply + (map (fn [p] (get-in db [:lengths (:interactorRef p)])) (participants (:data db)))))]
      {:db         (assoc-in db [:views :entities]
                             (let [entity-map (get-in db [:entity-map])]
                               (reduce (fn [v p]
                                         (let [p-length (get-in db [:lengths (:interactorRef p)])
                                               total    (apply + (map :length (vals v)))]
                                           (assoc v (keyword (:id p)) {:interactorRef (keyword (:interactorRef p))
                                                                       :id            (keyword (:id p))
                                                                       :length        p-length
                                                                       :type          (get-in entity-map [(:interactorRef p) :type :name])
                                                                       :label         (get-in interactors [(:interactorRef p) :label])
                                                                       :start-angle   (scale-fn total)
                                                                       :end-angle     (- (scale-fn (+ total p-length)) 10)})))
                                       {}
                                       (participants (:data db)))))
       :dispatch-n [
                    ;[:shape-defs]
                    [:shape-links]]})))

;(reg-event-db
;  :shape-defs
;  (fn [db]
;    (println
;      "DEFS"
;      (map (fn [entity]
;             ) (get-in db [:views :entities])))
;    db))

(defn parse-pos [str]
  (map (fn [part] (let [parsed (js/parseInt part)] (if-not (js/isNaN parsed) parsed nil))) (clojure.string/split str "-")))

(defn radial-scale [[lower-domain upper-domain] [lower-range upper-range]]
  (fn [input]
    (let [percent (/ input (- upper-domain lower-domain))]
      (+ lower-range (* percent (- upper-range lower-range))))))

(defn shortest-distance [d1 d2]
  (let [angle (mod (.abs js/Math (- d1 d2)) 360)]
    (if (> angle 180)
      (- 360 angle)
      angle)))

(reg-event-fx
  :shape-links
  (fn [{db :db}]
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

                           {:from          participant-id
                            :to            (get-in feature-map [(-> linkedFeatures first keyword) :participant-id])
                            :uid           (gensym)
                            :start-angle-1 (if (nil? from-pos-1)
                                             (- (:start-angle from-participant-view) 5)
                                             ((radial-scale
                                                [0 (:length from-participant-view)]
                                                [(:start-angle from-participant-view) (:end-angle from-participant-view)])
                                               from-pos-1))
                            :start-angle-2 (if (nil? from-pos-2)
                                             (:start-angle from-participant-view)
                                             ((radial-scale
                                                [0 (:length from-participant-view)]
                                                [(:start-angle from-participant-view) (:end-angle from-participant-view)])
                                               from-pos-2))
                            :end-angle-1   (if (nil? to-pos-1)
                                             (- (:start-angle to-participant-view) 5)
                                             ((radial-scale
                                                [0 (:length to-participant-view)]
                                                [(:start-angle to-participant-view) (:end-angle to-participant-view)])
                                               to-pos-1))
                            :end-angle-2   (if (nil? to-pos-2)
                                             (:start-angle to-participant-view)
                                             ((radial-scale
                                                [0 (:length to-participant-view)]
                                                [(:start-angle to-participant-view) (:end-angle to-participant-view)])
                                               to-pos-2))}))
                       feature-map)]
        {:db         (assoc db :feature-map feature-map
                               :link-views links)
         :dispatch-n [[:shape-features]
                      [:shape-superfam-features]]}))))





(reg-event-db
  :shape-features
  (fn [db]
    (let [features     (vals (get-in db [:feature-map]))
          entity-views (get-in db [:views :entities])]
      (update-in db [:views]
                 assoc
                 :features (map (fn [next]
                                  (let [[start-pos end-pos] (parse-pos (:pos (first (get-in next [:sequenceData]))))
                                        view  (get entity-views (:participant-id next))
                                        scale (radial-scale [0 (:length view)]
                                                            [(:start-angle view) (:end-angle view)])
                                        ]

                                    {:start-angle    (scale start-pos)
                                     :participant-id (:participant-id next)
                                     :end-angle      (scale end-pos)})) features)))))
(defn col->map [key col]
  (reduce (fn [total next]
            (assoc total (key next) next)) {} col))

(reg-event-db
  :shape-superfam-features
  (fn [db]
    (let [current-view      (col->map :interactorRef (vals (get-in db [:views :entities])))
          superfam-features (get-in db [:fts :superfamily])]
      (assoc-in db [:views :superfamily]
                (mapcat (fn [[feature-interactor feature-details]]
                          (let [feature-details  (vals feature-details)
                                participant-view (get-in current-view [(keyword feature-interactor)])
                                scale            (radial-scale [0 (:length participant-view)]
                                                               [(:start-angle participant-view) (:end-angle participant-view)])]
                            (map (fn [{:keys [START END METHOD] :as feature}]
                                   (if-not (= METHOD "Component")
                                     {:start-angle (scale (js/parseInt START))
                                      :end-angle   (scale (js/parseInt END))})) feature-details)))
                        superfam-features)))))

(reg-event-fx
  :success-fetch-complex
  (fn [{db :db} [_ response]]
    {:db         (assoc db :data response
                           :entity-map (vecmap->map [:id] (classes response)))
     :dispatch-n (concat
                   (map (fn [[{:keys [id] :as total} label]]
                          [:fetch-length id label]) (identifiers response))
                   (map (fn [[{:keys [id] :as total} label]]
                          [:fetch-superfamily id label]) (identifiers response)))}))


(reg-event-fx
  :success-fetch-length
  (fn [{db :db} [_ id label [{:keys [length]}]]]
    (let [new-db (assoc-in db [:lengths label] (js/parseInt length))]
      {:db       new-db
       :dispatch [:shape-entities]}
      #_(cond->
          {:db new-db}
          (>= (count (keys (get-in new-db [:lengths]))) (count (identifiers (get-in db [:data])))) (merge {:dispatch [:shape-entities]})))))

(reg-event-fx
  :failure-fetch-length
  (fn [{db :db} [_ id label response]]
    {:db       (assoc-in db [:lengths label] 20)
     :dispatch [:shape-entities]}))

(defn has-tag? [v]
  (fn [m]
    (= v (:tag m))))

(defn equals? [v]
  (fn [x] (= v x)))

(defn parse-superfamily-features [supfam-response]
  (->> (s/select [:content
                  s/ALL
                  :content
                  s/ALL
                  (has-tag? :SEGMENT)
                  :content
                  s/ALL
                  (has-tag? :FEATURE)
                  (s/collect-one [:attributes :id])
                  :content
                  s/ALL]
                 (xml->clj supfam-response))
       (reduce (fn [total [id data]]
                 (assoc-in total [id (:tag data)] (first (:content data)))) {})))

(reg-event-fx
  :success-fetch-superfamily
  (fn [{db :db} [_ id label response]]
    ;(println "LABEL" id )
    {:db (assoc-in db [:fts :superfamily label] (parse-superfamily-features response))}))


(reg-event-fx
  :fetch-superfamily
  (fn [{:keys [db]} [_ id label]]
    {:db         (assoc db :show-twirly true)
     :http-xhrio {:method          :get
                  ;:uri             (str "http://www.uniprot.org/uniprot/?format=json&columns=length,id,features&query=accession:" id)
                  :uri             (str "http://supfam.org/SUPERFAMILY/cgi-bin/das/up/features?segment=" id)
                  :timeout         30000
                  :response-format (ajax/text-response-format)
                  :on-success      [:success-fetch-superfamily id label]
                  :on-failure      [:failure-fetch-superfamily id label]}}))

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
    {:db         (assoc db :show-twirly true :complex-id id)
     :http-xhrio {:method          :get
                  :uri             (str "http://www.ebi.ac.uk/intact/complex-ws/export/" id)
                  :timeout         30000
                  :response-format (ajax/json-response-format {:keywords? true})
                  :on-success      [:success-fetch-complex]
                  :on-failure      [:bad-http-result]}}))


(reg-event-db
  :set-pinch-depth
  (fn [db [_ amount]]
    (assoc-in db [:options :pinch-depth] amount)))

(reg-event-db
  :set-pinch-percent
  (fn [db [_ amount]]
    (assoc-in db [:options :pinch-percent] amount)))

(reg-event-db
  :set-flare
  (fn [db [_ amount]]
    (assoc-in db [:options :flare] amount)))

(reg-event-db
  :toggle-inline-features
  (fn [db]
    (update-in db [:options :inline-features] not)))

(reg-event-db
  :highlight-entity
  (fn [db [_ id]]
    (if id
      (let [visible-links (map :uid (filter (fn [{:keys [from to]}]
                                              (or (= from id) (= to id))) (filter (fn [{:keys [start-angle-1 end-angle-1]}]
                                                                                    (> start-angle-1 end-angle-1)) (:link-views db))))
            visible-nodes (flatten (map (juxt :to :from) (filter (fn [{:keys [from to]}]
                                                                   (or (= from id) (= to id))) (filter (fn [{:keys [start-angle-1 end-angle-1]}]
                                                                                                         (> start-angle-1 end-angle-1)) (:link-views db)))))]
        (update db :flags assoc
                :highlight-entity id
                :visible-nodes visible-nodes
                :visible-links visible-links))
      (update db :flags assoc
              :highlight-entity nil
              :visible-nodes nil
              :visible-links nil))))

(reg-event-db
  :highlight-link
  (fn [db [_ id from to]]
    (if id
      (let [visible-links [id]
            visible-nodes [from to]]
        (update db :flags assoc
                :highlight-entity id
                :visible-nodes visible-nodes
                :visible-links visible-links))
      (update db :flags assoc
              :highlight-entity nil
              :visible-nodes nil
              :visible-links nil))))