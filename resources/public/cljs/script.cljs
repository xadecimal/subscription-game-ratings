(ns script)

(defn set-display
  [class display]
  (.forEach
   (.querySelectorAll js/document (str "." class))
   (fn[e] (set! (-> e .-style .-display) (name display)))))

(defn do-filter
  [e]
  (let [value (.-value e)
        checked (.-checked e)]
    (if checked
      (set-display value :table-row)
      (set-display value :none))))

(defn sort-table-by-column
  [table-id column-idx ascending]
  (let [table (-> js/document (.getElementById table-id))
        rows (-> js/Array (.from (-> table (.querySelectorAll ":scope > tbody > tr"))))]
    (-> rows (.sort
              (fn[row1 row2]
                (let [row1Value (-> (aget (.-cells row1) column-idx) .-textContent)
                      row2Value (-> (aget (.-cells row2) column-idx) .-textContent)
                      row1Num (if (= "" row1Value) 0 (js/parseFloat row1Value))
                      row2Num (if (= "" row2Value) 0 (js/parseFloat row2Value))]
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
