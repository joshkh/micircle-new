(ns micircle.core
    (:require [reagent.core :as reagent]
              [re-frame.core :as re-frame]
              [devtools.core :as devtools]
              [micircle.handlers]
              [micircle.subs]
              [micircle.routes :as routes]
              [micircle.views :as views]
              [micircle.config :as config]))


(defn dev-setup []
  (when config/debug?
    (enable-console-print!)
    (println "dev mode")
    (devtools/install!)))

(defn mount-root []
  (reagent/render [views/main-panel]
                  (.getElementById js/document "app")))

(defn ^:export init []
  (routes/app-routes)
  (re-frame/dispatch-sync [:initialize-db])
  (re-frame/dispatch [:fetch-complex "EBI-9008420"])
  ;(re-frame/dispatch [:fetch-complex "EBI-9691559"])
  (dev-setup)
  (mount-root))
