(ns com.xadecimal.subscription-game-ratings.push-square
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.zip :as z]
   [com.xadecimal.subscription-game-ratings.igdb :as igdb]
   [com.xadecimal.subscription-game-ratings.model :as m]
   [com.xadecimal.subscription-game-ratings.utils :as u]
   [hickory.core :as h]
   [hickory.select :as hs]
   [clojure.set :as set])
  (:import
   [java.time LocalDate ZoneId]
   [java.time.format DateTimeFormatter]
   [java.util Date Locale]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Push Square PS+ Games List

(def push-domain "https://www.pushsquare.com/")

(def push-html (u/slurp-memo "https://www.pushsquare.com/guides/all-ps-plus-games"))

(def push-ea-html (u/slurp-memo "https://www.pushsquare.com/guides/all-ea-play-games-on-ps5-ps4"))

(def push-hickory (-> push-html h/parse h/as-hickory))

(def push-ea-hickory (-> push-ea-html h/parse h/as-hickory))

(defn parse-push-date
  [push-date]
  (some-> push-date
          (->> (re-find #"(\d{1,2}\w{2}.\w{3}.\d{4}).*"))
          (second)
          (LocalDate/parse (DateTimeFormatter/ofPattern "d['st']['nd']['rd']['th'] MMM yyyy" Locale/US))
          (.atStartOfDay (ZoneId/of "UTC"))
          (.toInstant)
          (Date/from)))

(defn parse-ps-store-date
  [date]
  (some-> date
          (LocalDate/parse (DateTimeFormatter/ofPattern "MM/dd/yyyy" Locale/US))
          (.atStartOfDay (ZoneId/of "UTC"))
          (.toInstant)
          (Date/from)))

(defn get-push-game-details
  [url]
  (let [html (u/slurp-memo url)
        hickory (-> html h/parse h/as-hickory)
        release-date (some->> hickory
                              (hs/select-locs (hs/and (hs/tag "img")
                                                      (hs/class "flag")))
                              (first)
                              (z/up)
                              (z/right)
                              (z/node)
                              (str/trim)
                              (parse-push-date))
        user-score (some->> hickory
                            (hs/select (hs/and (hs/tag "span")
                                               (hs/class "score")))
                            (first)
                            (:content)
                            (first)
                            (parse-double)
                            (* 10))
        push-score (some->> hickory
                            (hs/select (hs/child (hs/class "our-review")
                                                 (hs/tag "a")))
                            (first)
                            (:content)
                            (first)
                            (edn/read-string)
                            (double)
                            (* 100))
        img (some->> hickory
                     (hs/select (hs/descendant (hs/class "info")
                                               (hs/attr "href")))
                     (first)
                     (:attrs)
                     (:href))
        platforms (set/union
                   #{}
                   (some->> hickory
                            (hs/select
                             (hs/and
                              (hs/tag "span")
                              (hs/class "sys")))
                            (first)
                            (:content)
                            (map (comp str/lower-case str/trim))
                            (filter #{"ps1" "ps2" "ps3" "ps4" "ps5" "psp"})
                            (map push-platform->platform-kw)
                            (set))
                   (some->> hickory
                            (hs/select
                             (hs/follow
                              (hs/and (hs/tag "dt")
                                      (hs/find-in-text #"Also Available On"))
                              (hs/tag "dd")))
                            (first)
                            (hs/select
                             (hs/child
                              (hs/tag "a")))
                            (map #(-> % (:content) (first)))
                            (map (comp str/lower-case str/trim))
                            (filter #{"ps1" "ps2" "ps3" "ps4" "ps5" "psp"})
                            (map push-platform->platform-kw)
                            (set)))]
    {:release-date release-date
     :user-score user-score
     :score push-score
     :img img
     :url url
     :platforms platforms}))

(defn get-ps-store-game-details
  [url]
  (let [html (u/slurp-memo url)
        hickory (-> html h/parse h/as-hickory)
        release-date (some->> hickory
                              (hs/select
                               (hs/follow
                                (hs/and (hs/tag "dt")
                                        (hs/find-in-text #"Release:"))
                                (hs/tag "dd")))
                              (first)
                              (:content)
                              (first)
                              (parse-ps-store-date))
        img (some->> hickory
                     (hs/select
                      (hs/descendant
                       (hs/and
                        (hs/tag "img")
                        (hs/attr "data-qa" #{"gameBackgroundImage#heroImage#image-no-js"}))))
                     (first)
                     (:attrs)
                     (:src))
        platforms (some->> hickory
                           (hs/select
                            (hs/follow
                             (hs/and (hs/tag "dt")
                                     (hs/find-in-text #"Platform:"))
                             (hs/tag "dd")))
                           (first)
                           (:content)
                           (first)
                           (str/trim)
                           (str/lower-case)
                           (push-platform->platform-kw)
                           (hash-set))]
    {:release-date release-date
     :img img
     :url url
     :platforms platforms}))

#_(get-ps-store-game-details "https://store.playstation.com/en-us/product/UP0006-CUSA24010_00-NHL94RPRESTANDED")

(defn push-platform->platform-kw
  [platform]
  (case platform
    "ps1" :ps1
    "ps2" :ps2
    "ps3" :ps3
    "ps4" :ps4
    "ps5" :ps5
    #_#_"psvr" :psvr
    #_#_"psvr2" :psvr2
    "psp" :psp))

(defn make-push-game-map
  [push-hickory-game-li subscription]
  (let [a (first (hs/select (hs/tag "a") push-hickory-game-li))
        pills (hs/select (hs/class "pill") push-hickory-game-li)
        url (str push-domain (-> a :attrs :href))
        push-game-details (get-push-game-details url)
        title (first (:content a))
        platforms (->> (mapcat :content pills)
                       (map (comp str/lower-case str/trim))
                       (filter #{"ps1" "ps2" "ps3" "ps4" "ps5" "psp"})
                       (map push-platform->platform-kw))
        release-date (:release-date push-game-details)
        igdb (igdb/find-igdb-game title release-date platforms)
        push-img (:img push-game-details)
        igdb-img (-> igdb (get "cover") (get "img"))]
    (m/make-game-map
     {:title title
      :platforms platforms
      :subscription subscription
      :release-date release-date
      :user-score (:user-score push-game-details)
      :score (:score push-game-details)
      :img (if (or
                (str/ends-with? push-img "ps1/cover_large.jpg")
                (str/ends-with? push-img "ps2/cover_large.jpg")
                (str/ends-with? push-img "ps3/cover_large.jpg")
                (str/ends-with? push-img "ps4/cover_large.jpg")
                (str/ends-with? push-img "ps5/cover_large.jpg")
                (str/ends-with? push-img "psp/cover_large.jpg"))
             igdb-img
             push-img)
      :url (:url push-game-details)
      :igdb igdb})))

(defn make-push-ea-game-map
  [push-hickory-game-li subscription]
  (let [a (first (hs/select (hs/tag "a") push-hickory-game-li))
        href (-> a :attrs :href)
        url (if (str/starts-with? href "https")
              href
              (str push-domain href))
        push-game-details (if (str/starts-with? url "https://store.playstation")
                            (get-ps-store-game-details url)
                            (get-push-game-details url))
        title (first (:content a))
        title (if (map? title)
                (-> title (:content) (first))
                title)
        release-date (:release-date push-game-details)
        platforms (:platforms push-game-details)
        igdb (igdb/find-igdb-game title release-date platforms)
        push-img (:img push-game-details)
        igdb-img (-> igdb (get "cover") (get "img"))]
    (m/make-game-map
     {:title title
      :platforms platforms
      :subscription subscription
      :release-date release-date
      :user-score (:user-score push-game-details)
      :score (:score push-game-details)
      :img (if (or
                (str/ends-with? push-img "ps1/cover_large.jpg")
                (str/ends-with? push-img "ps2/cover_large.jpg")
                (str/ends-with? push-img "ps3/cover_large.jpg")
                (str/ends-with? push-img "ps4/cover_large.jpg")
                (str/ends-with? push-img "ps5/cover_large.jpg")
                (str/ends-with? push-img "psp/cover_large.jpg"))
             igdb-img
             push-img)
      :url (:url push-game-details)
      :igdb igdb})))

(defn scrape-push-catalog
  [push-hickory catalog-id subscription]
  (->> push-hickory
       (hs/select
        (hs/follow
         (hs/id catalog-id)
         (hs/and (hs/tag "div")
                 (hs/class "games-filtered"))))
       (first)
       (hs/select
        (hs/descendant
         (hs/and (hs/tag "ul")
                 (hs/class "games")
                 (hs/class "games-style-list"))))
       (first)
       (:content)
       #_(take 31)
       (map #(make-push-game-map % subscription))))

(defn scrape-push-ea-catalog
  [push-hickory catalog-id subscription]
  (->> push-hickory
       (hs/select
        (hs/follow
         (hs/id catalog-id)
         (hs/tag "ul")))
       (first)
       (hs/select
        (hs/tag "li"))
       #_(take 31)
       (map #(make-push-ea-game-map % subscription))))

(defmethod m/scrape-catalog :extra
  [_]
  (->> (scrape-push-catalog push-hickory "all-ps-plus-extra-games-list" :extra)
       (reduce u/extract-id {})))

(defmethod m/scrape-catalog :premium
  [_]
  (->> (scrape-push-catalog push-hickory "all-ps-plus-premium-games-list" :premium)
       (reduce u/extract-id {})))

(defmethod m/scrape-catalog :ea-play-ps
  [_]
  (->> (scrape-push-ea-catalog push-ea-hickory "all-ea-play-games-on-ps5-ps4" :ea-play-ps)
       (reduce u/extract-id {})))

;;;; Push Square PS+ Games List
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_(m/scrape-catalog :extra)

#_(m/scrape-catalog :premium)

#_(m/scrape-catalog :ea-play-ps)
