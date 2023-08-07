(ns com.xadecimal.subscription-game-ratings.model)

(defn make-game-map
  [{:keys [title subscription platforms release-date
           user-score score img url igdb]}]
  (cond->
      {:id (get igdb "id")
       :title title
       :subscriptions #{subscription}
       :platforms (set platforms)
       :url url
       :igdb igdb}
    release-date
    (assoc :release-date release-date)
    user-score
    (assoc :user-score user-score)
    score
    (assoc :score score)
    img
    (assoc :img img)))

(defmulti scrape-catalog identity)
