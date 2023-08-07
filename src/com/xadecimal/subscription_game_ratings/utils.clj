(ns com.xadecimal.subscription-game-ratings.utils)

;;;;;;;;;;;;;;
;;;; Utils

(defn date->unix
  [date]
  (some-> date
          (.getTime)
          (/ 1000)))

(def unix-week
  604800)

(defonce
  slurp-memo
  (memoize
   (fn [url]
     (slurp url))))

(defn
  inner-merge
  [a b]
  (cond
    (set? a)
    (into a b)
    :else
    a))

(defn outer-merge
  [a b]
  (merge-with inner-merge a b))

(defn extract-id
  [acc e]
  (if (contains? acc (:id e))
    acc
    (assoc acc (:id e) e)))

;;;; Utils
;;;;;;;;;;;;;;
