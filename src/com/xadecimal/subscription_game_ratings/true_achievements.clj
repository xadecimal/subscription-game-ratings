(ns com.xadecimal.subscription-game-ratings.true-achievements 
  (:require
   [clojure.string :as str]
   [com.xadecimal.subscription-game-ratings.igdb :as igdb]
   [com.xadecimal.subscription-game-ratings.model :as m]
   [com.xadecimal.subscription-game-ratings.utils :as u]
   [hickory.core :as h]
   [hickory.select :as hs])
  (:import
   [java.time LocalDate ZoneId]
   [java.time.format DateTimeFormatter]
   [java.util Date Locale]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; True Achievements Xbox Games List

(def game-pass-domain "https://www.trueachievements.com")

(def pc-game-pass-url "https://www.trueachievements.com/pc-game-pass/games")

(def xbox-game-pass-url "https://www.trueachievements.com/xbox-game-pass/games")

(def ea-play-xb-pass-url "https://www.trueachievements.com/ea-play/games")

(defn get-all-hickory-pages
  [url]
  (let [first-page (-> url u/slurp-memo h/parse h/as-hickory)
        urls (->> first-page
                  (hs/select
                   (hs/descendant
                    (hs/and
                     (hs/tag "div")
                     (hs/class "bottomlinks"))
                    (hs/tag "ul")))
                  (first)
                  (hs/select
                   (hs/and
                    (hs/tag "li")
                    (hs/not (hs/class "current"))
                    (hs/not (hs/class "h-ellip"))
                    (hs/not (hs/class "prevnext"))))
                  (map #(-> % :content first :attrs :href))
                  (map #(str game-pass-domain %)))]
    (into [first-page] (mapv #(-> % u/slurp-memo h/parse h/as-hickory) urls))))

(def pc-game-pass-hickory-pages (get-all-hickory-pages pc-game-pass-url))

(def xbox-game-pass-hickory-pages (get-all-hickory-pages xbox-game-pass-url))

(def ea-play-xb-hickory-pages (get-all-hickory-pages ea-play-xb-pass-url))

(defn parse-true-date
  [date]
  (some-> date
          (LocalDate/parse (DateTimeFormatter/ofPattern "dd MMMM yyyy" Locale/US))
          (.atStartOfDay (ZoneId/of "UTC"))
          (.toInstant)
          (Date/from)))

(defn true-sub->subscription-kw
  [sub]
  (case sub
    "EA Play"
    :ea-play-xb
    "PC Game Pass"
    :pc-gp
    "Xbox Game Pass"
    :xbox-gp
    nil))

(defn scrape-true-game
  [url]
  (let [page (-> url u/slurp-memo h/parse h/as-hickory)
        game-info (hs/select
                   (hs/and (hs/tag "dl")
                           (hs/class "game-info"))
                   page)
        release-date (some->> game-info
                              first
                              (hs/select
                               (hs/follow
                                (hs/and (hs/tag "dt")
                                        (hs/find-in-text #"Release"))
                                (hs/tag "dd")))
                              (first)
                              (:content)
                              (first)
                              (parse-true-date))
        platforms (some->> game-info
                           first
                           (hs/select
                            (hs/follow
                             (hs/and (hs/tag "dt")
                                     (hs/find-in-text #"Platform"))
                             (hs/tag "dd")))
                           (first)
                           (hs/select
                            (hs/child
                             (hs/tag "a")))
                           (map #(-> % (:content) (first)))
                           (set))
        user-score (some->> page
                            (hs/select
                             (hs/and
                              (hs/tag "span")
                              (hs/has-child
                               (hs/and
                                (hs/tag "i")
                                (hs/class "fa-star")))))
                            (first)
                            (:content)
                            (second)
                            (re-find #"\d.{0,1}\d*")
                            (parse-double)
                            (* 20))
        img (some->> page
                     (hs/select
                      (hs/descendant
                       (hs/class "info")
                       (hs/class "img")
                       (hs/tag "img")))
                     (first)
                     (:attrs)
                     (:src)
                     (str game-pass-domain))
        subscriptions (some->> game-info
                               first
                               (hs/select
                                (hs/follow
                                 (hs/and (hs/tag "dt")
                                         (hs/find-in-text #"Notes:"))
                                 (hs/tag "dd")))
                               (first)
                               (hs/select
                                (hs/child
                                 (hs/tag "a")))
                               (map #(-> % (:content) (first)))
                               (map true-sub->subscription-kw)
                               (remove nil?)
                               (set))]
    {:platforms platforms
     :release-date release-date
     :user-score user-score
     :img img
     :url url
     :subscriptions subscriptions}))

(defn true-platform->platform-kw
  [platform]
  (case platform
    "windows" :pc
    "xbox series x|s" :xsx
    "xbox one" :xone
    "xbox 360" :x360))

(defn make-xbox-game-map
  [{[title] :content
    {:keys [href]} :attrs} subscription]
  (let [href (str game-pass-domain href)
        true-game-details (scrape-true-game href)
        platforms (->> true-game-details
                       :platforms
                       (map (comp str/lower-case str/trim))
                       (filter #{"windows"
                                 "xbox series x|s"
                                 "xbox one"
                                 "xbox 360"})
                       (map true-platform->platform-kw))
        release-date (:release-date true-game-details)
        igdb (igdb/find-igdb-game title release-date platforms)]
    (when (contains? (:subscriptions true-game-details) subscription)
      (m/make-game-map
       {:title title
        :platforms platforms
        :subscription (cond
                        (= :pc-gp subscription)
                        subscription
                        (:ea-play-xb? true-game-details)
                        :ea-play-xb
                        :else
                        subscription)
        :release-date release-date
        :user-score (:user-score true-game-details)
        :img (-> igdb (get "cover") (get "img"))
        :url (:url true-game-details)
        :igdb igdb}))))

(defn scrape-xbox-catalog
  [subscription hickory]
  (->> hickory
       (hs/select
        (hs/descendant
         (hs/id "oGameList")
         (hs/tag "tbody")
         (hs/and (hs/tag "tr")
                 (hs/or (hs/class "even")
                        (hs/class "odd")))
         (hs/and (hs/tag "td")
                 (hs/class "game"))))
       (map :content)
       (map first)
       #_(take 31)
       (map #(make-xbox-game-map % subscription))
       (remove nil?)))

(defmethod m/scrape-catalog :pc-gp
  [_]
  (->> (mapcat (partial scrape-xbox-catalog :pc-gp) pc-game-pass-hickory-pages)
       (reduce u/extract-id {})))

(defmethod m/scrape-catalog :xbox-gp
  [_]
  (->> (mapcat (partial scrape-xbox-catalog :xbox-gp) xbox-game-pass-hickory-pages)
       (reduce u/extract-id {})))

(defmethod m/scrape-catalog :ea-play-xb
  [_]
  (->> (mapcat (partial scrape-xbox-catalog :ea-play-xb) ea-play-xb-hickory-pages)
       (reduce u/extract-id {})))

;;;; True Achievements Xbox Games List
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_(m/scrape-catalog :pc-gp)

#_(m/scrape-catalog :xbox-gp)

#_(m/scrape-catalog :ea-play-xb)
