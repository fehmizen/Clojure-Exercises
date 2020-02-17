(ns mishmash
  (:require [clojure.string :as str]))

(defn factorial [n]
  (apply * (range 1 (inc n))))

(defn binomial[n k]
  (/ (factorial n) (* (factorial k) (factorial(- n k)))))

(defn pascal [x]
  (if (< x 0) "invalid input"
  (str/join " "(map #(binomial (dec (+ 1 x)) %) (range (+ 1 x))))))

(defn isNeg [num]
  (cond
    (< num 0) true
    :else false))

(def romans [[1000 "M"] [900 "CM"] [500 "D" ] [400 "CD"] [100 "C"]
            [90 "XC"] [50 "L"] [40 "XL"] [10 "X" ]
            [9 "IX"] [5 "V"] [4 "IV"] [1 "I" ]])

(defn write-roman [num]
  (if (< num 1) "invalid input"
  (if (> num 3999) "invalid input"
  (loop [answer "" number num [[key val ]& more] romans]
    (if-not val answer
       (recur (apply str (cons answer (repeat (quot number key) val )))
          (rem number key) more))))))

(def rmap {\M 1000, \D 500, \C 100, \L 50, \X 10, \V 5, \I 1})

(defn roman-convert [x y]
  (if (> x (* y 4))
    (- x y)
    (+ y x))
  )

(defn read-roman [roman]
  (def one (first roman))
  (def flip (reverse roman))
  (def look(map rmap flip))

  (if (contains? rmap one) (reduce roman-convert look) "invalid input"))

(defn -main [& args]
  (if (not= (count args) 2) (println "invalid input")
    ((def func (first args))
     (def params  (second args))

     (if (= func "pascal")
       ((try
          (println (pascal (Integer/parseInt params)))
          (catch Exception e (println "invalid input"))))
       )
     (if (= func "write-roman")
       ((try
          (println (write-roman (Integer/parseInt params)))
          (catch Exception e (println "invalid input")))
        ))
     (if (= func "read-roman")
       ((try
          (println (read-roman (str(second args)) ))
          (catch Exception e "invalid input")))
       ))
    )

  )
;(def romans (sorted-map-by > 1 "I", 4 "IV", 5 "V", 9 "IX", 10 "X",
;                           40 "XL", 50 "L", 90 "XC", 100 "C",
;                           400 "CD", 500 "D", 900 "CM", 1000 "M"))


;(defn -main [& args] ,,,
;  (let [func (first *command-line-args*)
;        param1 (second *command-line-args*)
;        rest (rest *command-line-args*)]
;
;    (if (not= (checking func) false)
;      (if (and (= "pascal" func) (< (count rest) 2))
;        (try
;          (pascal (Integer/parseInt param1))
;          (catch NumberFormatException e (println "invalid input")))
;        )
;      (if (= "write-roman" func)
;        (try
;          (println (write-roman (Integer/parseInt param1)))
;          (catch NumberFormatException e (println "invalid input")))
;        )
;      (if (= "read-roman" func)
;        (try
;          (read param1)
;          (catch NumberFormatException e (println "invalid input")))
;        )
;      )
;    (if (or (= (checking func) false) (> (count rest) 1))
;      (println "invalid input"))))
