(ns script)

(defn get-checked
  [checkbox-name]
  (->> (str "input[name=\"" checkbox-name "\"]:checked")
       (.querySelectorAll js/document)
       (map #(.-value %))))

(defn contains-one-of-class?
  [el classes]
  (boolean (some #(-> el .-classList (.contains %)) classes)))

(defn set-display
  [el display]
  (set! (-> el .-style .-display) (name display)))

(defn do-filter
  []
  (let [table (-> js/document (.getElementById "table-catalog"))
        rows (-> table (.querySelectorAll ":scope > tbody > tr"))
        checked-subs (get-checked "subs")
        checked-platforms (get-checked "platforms")]
    (.forEach rows
              (fn[row]
                (if (and (contains-one-of-class? row checked-subs)
                         (contains-one-of-class? row checked-platforms))
                  (set-display row :table-row)
                  (set-display row :none))))))

(defn get-text-content
  [row column-idx]
  (-> (aget (.-cells row) column-idx) .-textContent))

(defn parse-float
  [str]
  (if (= "" str) 0 (js/parseFloat str)))

(defn sort-table-by-column
  [table-id column-idx ascending]
  (let [table (-> js/document (.getElementById table-id))
        rows (-> js/Array (.from (-> table (.querySelectorAll ":scope > tbody > tr"))))]
    (-> rows (.sort
              (fn[row1 row2]
                (let [row1Value (get-text-content row1 column-idx)
                      row2Value (get-text-content row2 column-idx)
                      row1Num (parse-float row1Value)
                      row2Num (parse-float row2Value)]
                  (if ascending (- row1Num row2Num) (- row2Num row1Num))))))
    (.forEach rows
              (fn[row]
                (-> (aget (-> table .-tBodies) 0)
                    (.appendChild row))))))

(defn do-sort
  []
  (let [sort-el (-> js/document (.getElementById "sort"))
        order-el (-> js/document (.getElementById "order"))
        column (-> sort-el .-value js/parseInt)
        order (-> order-el .-value)
        ascending (if (= order "asc") true false)]
    (sort-table-by-column "table-catalog" column ascending)))

;; export function to use from JavaScript:
(set! (.-do_filter js/window) do-filter)
(set! (.-do_sort js/window) do-sort)
