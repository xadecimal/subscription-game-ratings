(ns com.xadecimal.subscription-game-ratings
  (:require
   [clojure.java.io :as io]
   [com.xadecimal.subscription-game-ratings.igdb :as igdb]
   [com.xadecimal.subscription-game-ratings.model :as m]
   [com.xadecimal.subscription-game-ratings.push-square]
   [com.xadecimal.subscription-game-ratings.true-achievements]
   [com.xadecimal.subscription-game-ratings.utils :as u]
   [selmer.parser :as html])
  (:import
   [java.time Instant ZoneId]
   [java.time.format DateTimeFormatter]
   [java.util Locale]))

;;;;;;;;;;;;;;;;;;;;
;;;; Get ratings

(def ratings
  (merge-with
   u/outer-merge
   (m/scrape-catalog :extra)
   (m/scrape-catalog :premium)
   (m/scrape-catalog :xbox-gp)
   (m/scrape-catalog :pc-gp)
   (m/scrape-catalog :ea-play-ps)
   (m/scrape-catalog :ea-play-xb)))

#_(get ratings 113598)

;;;; Get ratings
;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;
;;;; Generate HTML

(html/set-resource-path! (io/resource "html"))

#_(html/cache-off!)

(->>
 (html/render-file
  "subscription-game-ratings.html"
  {:catalogs (->> ratings
                  (vals)
                  (sort-by #(get (:igdb %) "total_rating")
                           #(> (or %1 Integer/MIN_VALUE) (or %2 Integer/MIN_VALUE))))
   :name "Title"
   :igdb {:name "IGDB Title"
          :total-rating "IGDB Total Rating (user+critics)"
          :rating "IGDB User Rating"}
   :push {:score "Push Score"
          :user-score "Push/True User Score"}
   :platforms (->> igdb/igdb-platform-map
                   keys
                   (map name)
                   (sort))
   :subs ["extra" "premium" "pc-gp" "xbox-gp" "ea-play-ps" "ea-play-xb"]
   :last-updated (-> (DateTimeFormatter/ofPattern "yyyy/MM/dd z" Locale/US)
                     (.withZone (ZoneId/of "UTC"))
                     (.format (Instant/now)))})
 (#(let [output "./resources/public/index.html"]
     (io/make-parents output)
     (spit output %))))

(System/exit 0)

;;;; Generate HTML
;;;;;;;;;;;;;;;;;;;;;;
