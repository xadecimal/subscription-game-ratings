(ns com.xadecimal.subscription-game-ratings
  (:require
   [babashka.pods :as pods]
   [cheshire.core :as json]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.zip :as z]
   [org.httpkit.client :as http]
   [selmer.parser :as html])
  (:import
   [java.time Instant LocalDate ZoneId]
   [java.time.format DateTimeFormatter]
   [java.util Date Locale]))

(pods/load-pod 'retrogradeorbit/bootleg "0.1.9")

(require '[pod.retrogradeorbit.bootleg.utils :as bootleg])

(require '[pod.retrogradeorbit.hickory.select :as hs])

;;;;;;;;;;;;;;
;;;; Utils

(defn date->unix
  [date]
  (some-> date
          (.getTime)
          (/ 1000)))

(def unix-week
  604800)

;;;; Utils
;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;
;;;; Fuzzy Matching

(defn normalize
  [str]
  (-> str
      (str/lower-case)
      (str/replace #"\s+" " ") ; make whitespace uniform
      (str/replace #"\(.*\)" "") ; remove things between parenthesis
      (str/replace #"'\S+" "") ; remove apostrophes
      (str/replace #"[^\sa-zA-Z0-9]" "") ; remove all punctuation
      ;; remove playstation specific wording
      (str/replace #" - PS4 & PS5" "")
      (str/replace #"PS4 & PS5" "")
      (str/replace #" - PS5 & PS4" "")
      (str/replace #"PS5 & PS4" "")
      (str/replace #" - PS4" "")
      (str/replace #"PS4" "")
      (str/replace #"PS5" "")
      (str/replace #" - PS5" "")
      (str/replace #" - PlayStation4 Edition" "")
      (str/replace #" - PlayStation5 Edition" "")
      ;; make latin numerals uniform
      (str/replace #"\bi\b" "1")
      (str/replace #"\bii\b" "2")
      (str/replace #"\biii\b" "3")
      (str/replace #"\biv\b" "4")
      (str/replace #"\bv\b" "5")
      (str/replace #"\bvi\b" "6")
      (str/replace #"\bvii\b" "7")
      (str/replace #"\bviii\b" "8")
      (str/replace #"\bix\b" "9")
      (str/replace #"\bx\b" "10")
      (str/replace #"\bxi\b" "11")
      (str/replace #"\bxii\b" "12")
      (str/replace #"\bxiii\b" "13")
      (str/replace #"\bxiv\b" "14")
      (str/replace #"\bxv\b" "15")
      (str/replace #"\bxvi\b" "16")
      (str/replace #"\bxvii\b" "17")
      (str/replace #"\bxviii\b" "18")
      (str/replace #"\bxix\b" "19")
      (str/replace #"\bxx\b" "20")
      (str/replace #"\bxxi\b" "21")
      (str/replace #"\bxxii\b" "22")
      (str/replace #"\bxxiii\b" "23")
      (str/trim)))

(defn lcs
  "Longest Common Substring
   from: https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Longest_common_substring#Clojure"
  [str1 str2]
  (loop [s1 (seq str1), s2 (seq str2), len 0, maxlen 0]
    (cond
      (>= maxlen (count s1)) maxlen
      (>= maxlen (+ (count s2) len)) (recur (rest s1) (seq str2) 0 maxlen)
      :else (let [a (nth s1 len ""), [b & s2] s2, len (inc len)]
              (if (= a b)
                (recur s1 s2 len (if (> len maxlen) len maxlen))
                (recur s1 s2 0 maxlen))))))

(defn lcs-similarity
  "Inspired by http://yomguithereal.github.io/talisman/metrics/#lcs"
  [str1 str2]
  (/ (lcs str1 str2)
     (max (count str1) (count str2))))

(defn lcs-distance
  [str1 str2]
  (- 1 (lcs-similarity str1 str2)))

(defn exact-match
  [str1 str2]
  (= (normalize str1)
     (normalize str2)))

;;;; Fuzzy Matching
;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Push Square PS+ Games List

(def push-domain "https://www.pushsquare.com/")

(def push-html (slurp "https://www.pushsquare.com/guides/all-ps-plus-games"))

(def push-hickory (bootleg/convert-to push-html :hickory))

(defn parse-push-date
  [push-date]
  (some-> push-date
          (->> (re-find #"(\d{1,2}\w{2}.\w{3}.\d{4}).*"))
          (second)
          (LocalDate/parse (DateTimeFormatter/ofPattern "d['st']['nd']['rd']['th'] MMM yyyy" Locale/US))
          (.atStartOfDay (ZoneId/of "UTC"))
          (.toInstant)
          (Date/from)))

(defonce get-push-game-details
  (memoize
   (fn get-push-game-details
     [url]
     (let [html (slurp url)
           hickory (bootleg/convert-to html :hickory)
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
                        (:href))]
       {:release-date release-date
        :user-score user-score
        :score push-score
        :img img
        :url url}))))

(defn make-push-game-map
  [push-hickory-game-li subscription]
  (let [a (first (hs/select (hs/tag "a") push-hickory-game-li))
        pills (hs/select (hs/class "pill") push-hickory-game-li)
        url (str push-domain (-> a :attrs :href))
        push-game-details (get-push-game-details url)]
    {:title (first (:content a))
     :platforms (mapcat :content pills)
     :subscription subscription
     :push push-game-details}))

(defn scrape-catalog
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
       (map #(make-push-game-map % subscription))))

(def push-game-catalog
  (scrape-catalog push-hickory "all-ps-plus-extra-games-list" :extra))

(def push-classics-catalog
  (scrape-catalog push-hickory "all-ps-plus-premium-games-list" :premium))

;;;; Push Square PS+ Games List
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;
;;;; IGDB API

(def igdb-access-token-url "https://id.twitch.tv/oauth2/token")

(def igdb-client-id "vtflt38ztq8znizdm96gixdu4uccze")

(def igdb-client-secret "gksuczvwva2fol79vfyuay7r0tr00s")

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
   :psvr2 390
   :psvr 165})

(defn platform-ids->platforms-str
  [platform-ids]
  (str "(" (str/join "," platform-ids) ")"))

(defn platforms-kws->ids [platforms]
  (into #{} (map igdb-platform-map) platforms))

(defn drop-last-word [title]
  (->> #"(?U)\s+"
       (str/split title)
       drop-last
       (str/join " ")))

(defn filter-by-exact
  [title coll]
  (some->> coll
           (filter #(exact-match title (get % "name")))))

(defn filter-by-date
  [release-date-unix coll]
  (some-> coll
          (cond->> release-date-unix
            (filter #(->> (get % "release_dates")
                          (some (fn[%]
                                  ;; If within 1 week of release
                                  (when (get % "date")
                                    (< (- release-date-unix unix-week)
                                       (get % "date")
                                       (+ release-date-unix unix-week))))))))))

(defn filter-by-rating
  [coll]
  (filter #(or (get % "total_rating")
               (get % "rating")
               (get % "parent_game.total_rating")
               (get % "parent_game.rating")) coll))

(defn sort-by-lcs
  [title coll]
  (sort-by #(lcs-distance title (get % "name")) coll))

(defn best-first
  [title matches]
  (or (->> matches (filter-by-rating) (sort-by-lcs title) (first))
      (->> matches (sort-by-lcs title) (first))))

(defn most-similar-of-all
  [title found-so-far release-date]
  (or (->> found-so-far (filter-by-exact title) (first))
      (->> found-so-far (filter-by-date (date->unix release-date)) (first))
      (->> found-so-far (sort-by-lcs title) (first))))

(defn find-igdb-game
  ([title release-date platforms]
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
           release-date-unix (date->unix release-date)
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
     (most-similar-of-all title found-so-far release-date))))

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

(defn retrieve-igdb-catalog-ratings
  [push-catalog platforms]
  (map-indexed
   (fn[i {:keys [title]
          {:keys [release-date]} :push}]
     (println i ": " title release-date)
     (find-igdb-game title release-date platforms))
   push-catalog))

(def igdb-game-catalog
  (retrieve-igdb-catalog-ratings push-game-catalog [:ps5 :ps4 :psvr2 :psvr]))

(def igdb-classics-catalog
  (retrieve-igdb-catalog-ratings push-classics-catalog [:ps4 :ps3 :ps2 :ps1 :psp]))

;;;; IGDB API
;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;
;;;; Get ratings

(defn make-catalog-ratings
  [push-catalog igdb-catalog]
  (map
   (fn [{:keys [title platforms subscription push]} igdb]
     {:title title
      :platforms platforms
      :subscription subscription
      :igdb igdb
      :push push})
   push-catalog igdb-catalog))

(def game-catalog-ratings
  (make-catalog-ratings push-game-catalog igdb-game-catalog))

(def classics-catalog-ratings
  (make-catalog-ratings push-classics-catalog igdb-classics-catalog))

;;;; Get ratings
;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;
;;;; Generate HTML

(html/set-resource-path! (io/resource "html"))

(html/add-tag!
 :ifendswith
 (fn [args context-map content]
   (let [args (map #(html/resolve-arg % context-map) args)]
     (if (str/ends-with? (first args) (second args))
       (-> content :ifendswith :content)
       (-> content :else :content))))
 :else :endifendswith)

#_(html/cache-off!)

(->>
 (html/render-file
  "subscription-game-ratings.html"
  {:catalogs (->> game-catalog-ratings
                  (concat classics-catalog-ratings)
                  (take 31)
                  (sort-by #(get (:igdb %) "total_rating")
                           #(> (or %1 Integer/MIN_VALUE) (or %2 Integer/MIN_VALUE))))
   :name "Title"
   :igdb {:name "IGDB Title"
          :total-rating "IGDB Total Rating (user+critics)"
          :rating "IGDB User Rating"}
   :push {:score "Push Score"
          :user-score "Push User Score"}
   :last-updated (-> (DateTimeFormatter/ofPattern "yyyy/MM/dd z" Locale/US)
                     (.withZone (ZoneId/of "UTC"))
                     (.format (Instant/now)))})
 (#(let [output "./resources/public/index.html"]
     (io/make-parents output)
     (spit output %))))

(System/exit 0)

;;;; Generate HTML
;;;;;;;;;;;;;;;;;;;;;;
