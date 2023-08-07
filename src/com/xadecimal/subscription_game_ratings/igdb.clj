(ns com.xadecimal.subscription-game-ratings.igdb 
  (:require
   [cheshire.core :as json]
   [clojure.string :as str]
   [com.xadecimal.subscription-game-ratings.fuzzy-matching :as fm]
   [com.xadecimal.subscription-game-ratings.utils :as u]
   [org.httpkit.client :as http]))

;;;;;;;;;;;;;;;;;
;;;; IGDB API

(def igdb-access-token-url "https://id.twitch.tv/oauth2/token")

(def igdb-client-id "vtflt38ztq8znizdm96gixdu4uccze")

(def igdb-client-secret (or (System/getenv "IGDB_CLIENT_SECRET")
                            (throw
                             (ex-info
                              "Environment var IGDB_CLIENT_SECRET must be set."
                              {}))))

(def igdb-games-api "https://api.igdb.com/v4/games")

(def igdb-access-token
  @(http/post
    igdb-access-token-url
    {:query-params {"client_id" igdb-client-id
                    "client_secret" igdb-client-secret
                    "grant_type" "client_credentials"}}))

(def igdb-headers
  {"Client-ID" igdb-client-id
   "Authorization" (str "Bearer "
                        (-> igdb-access-token
                            (get :body)
                            json/decode
                            (get "access_token")))})

(let [last-call (atom 0)]
  (defonce call-igdb
    (memoize
     (fn call-igdb
       [igdb-api-url body]
       ;; We can only call igdb with 4 TPS, so we sleep the difference
       ;; if it's not been 250ms since the last time we called igdb
       (let [now (System/currentTimeMillis)
             next-allowed-time (+ @last-call 250)]
         (when (< now next-allowed-time)
           (Thread/sleep (- next-allowed-time now))))
       (let [ret (some-> @(http/post
                           igdb-api-url
                           {:headers igdb-headers
                            :body body})
                         (:body)
                         (json/decode))]
         (reset! last-call (System/currentTimeMillis))
         ret)))))

(def igdb-platform-map
  {:ps1 7
   :ps2 8
   :ps3 9
   :ps4 48
   :ps5 167
   :psp 38
   #_#_:psvr2 390
   #_#_:psvr 165
   :pc 6
   :xbox 11
   :x360 12
   :xone 49
   :xsx 169})

(defn platform-ids->platforms-str
  [platform-ids]
  (str "(" (str/join "," platform-ids) ")"))

(defn platforms-kws->ids
  [platforms]
  (into #{} (map igdb-platform-map) platforms))

(defn drop-last-word
  [title]
  (->> #"(?U)\s+"
       (str/split title)
       drop-last
       (str/join " ")))

(defn filter-by-exact
  [title coll]
  (some->> coll
           (filter #(fm/exact-match title (get % "name")))))

(defn filter-by-date
  [release-date-unix coll]
  (some-> coll
          (cond->> release-date-unix
            (filter #(->> (get % "release_dates")
                          (some (fn[%]
                                  ;; If within 1 week of release
                                  (when (get % "date")
                                    (< (- release-date-unix u/unix-week)
                                       (get % "date")
                                       (+ release-date-unix u/unix-week))))))))))

(defn filter-by-rating
  [coll]
  (filter #(or (get % "total_rating")
               (get % "rating")
               (get % "parent_game.total_rating")
               (get % "parent_game.rating")) coll))

(defn sort-by-lcs
  [title coll]
  (sort-by #(fm/lcs-distance title (get % "name")) coll))

(defn best-first
  [title matches]
  (or (->> matches (filter-by-rating) (sort-by-lcs title) (first))
      (->> matches (sort-by-lcs title) (first))))

(defn most-similar-of-all
  [title found-so-far release-date]
  (or (->> found-so-far (filter-by-exact title) (first))
      (->> found-so-far (filter-by-date (u/date->unix release-date)) (first))
      (->> found-so-far (sort-by-lcs title) (first))))

(let [i (atom 0)]
  (defn find-igdb-game
    ([title release-date platforms]
     (println @i ": " title release-date platforms)
     (swap! i inc)
     (some-> (find-igdb-game title release-date platforms [])
             (update "cover"
                     #(assoc %
                             "img"
                             (format
                              "https://images.igdb.com/igdb/image/upload/t_cover_big/%s.png"
                              (get % "image_id"))))))
    ([title release-date platforms found-so-far]
     (if (seq title)
       (let [platform-ids (platforms-kws->ids platforms)
             platforms-str (platform-ids->platforms-str platform-ids)
             release-date-unix (u/date->unix release-date)
             query (str "fields name,url,total_rating,rating,release_dates.date,release_dates.platform,parent_game.total_rating,parent_game.rating,cover.image_id; search \""
                        title "\"; limit 10; where platforms = " platforms-str " & category = (0,3,4,8,9,10,11,14);")
             found (call-igdb igdb-games-api query)
             matches-exactly (filter-by-exact title found)
             matches-release-date (filter-by-date release-date-unix found)]
         (if-let [matches (seq matches-exactly)]
           (if-let [match (->> matches (filter-by-date release-date-unix) (seq))]
             (best-first title match)
             (best-first title matches))
           (if-let [matches (seq matches-release-date)]
             (best-first title matches)
             (recur (drop-last-word title)
                    release-date
                    platforms
                    (into found-so-far found)))))
       (most-similar-of-all title found-so-far release-date)))))

#_(find-igdb-game "Death End Re;Quest 2"
                  (parse-push-date "13th Feb 2020")
                  [:ps4 :ps5])

#_(find-igdb-game "Alien Rage"
                  (parse-push-date "21st Oct 2013")
                  [:ps1 :ps2 :ps3 :ps4 :psp])

#_(find-igdb-game "Bassmaster Fishing 2022"
                  (parse-push-date "28th Oct 2021, $19.99")
                  [:ps1 :ps2 :ps3 :ps4 :psp])

#_(find-igdb-game "Werewolves Within"
                  (parse-push-date "6th Dec 2016")
                  [:ps4 :ps5 :psvr :psvr2])

#_(find-igdb-game "Far Cry 3: Blood Dragon Classic"
                  nil
                  [:ps4 :ps5 :psvr :psvr2])

;;;; IGDB API
;;;;;;;;;;;;;;;;;
