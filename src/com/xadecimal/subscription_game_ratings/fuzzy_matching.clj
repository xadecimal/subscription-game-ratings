(ns com.xadecimal.subscription-game-ratings.fuzzy-matching 
  (:require
   [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;
;;;; Fuzzy Matching

(defn normalize
  [str]
  (some-> str
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
      (str/replace #"(Windows)" "")
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
  (and str1
       str2
       (= (normalize str1)
          (normalize str2))))

;;;; Fuzzy Matching
;;;;;;;;;;;;;;;;;;;;;;;
