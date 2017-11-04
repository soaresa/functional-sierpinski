(ns sierpinski.imperative-solution)

;;;; First solution with wrong approach - don't do it

(def tri-char "1")

(def edge-char "_")

(defn segment [n c] (apply str (take n (repeat c))))

(defn tri [n] (segment n tri-char))

(defn edge [n] (segment n edge-char))

(defn build-tri [i b h le re lines]
  (if (< h 1)
    lines
    (recur (inc i) (- b 2) (dec h) (inc le) (inc re)
           (update lines i str (edge le) (tri b) (edge re)))))

(defn build-frac
  ([n]
   (let [b (/ 64 (Math/pow 2 n))
         h (/ b 2)]
     (build-frac n b h 0 0 0 (sorted-map))))
  ([n b h a le re r]
   (if (= n 0)
     (build-tri (* a h) (dec b) h le re r)
     (let [i (dec n)
           p (Math/pow 2 i)
           s (* p h)]
       (->> r
            (build-frac i b h a le 0)
            (build-frac i b h a 1 re)
            (build-frac i b h (+ a p) (+ le s) (+ re s)))))))

(defn print-fractal [n]
  (let [fr (->> n build-frac reverse vals)]
    (doseq [line fr]
      (println line))))

