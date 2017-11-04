(ns sierpinski.declarative-solution)

;;;; Second solution with declarative approach

;;; This solution prints the Sierpinski Triangle fractal in ASCII art.
;;; The minimum fractal pattern considered is formed by 3 blocks.
;;; Each block hosts the same pattern:
;;;        _________
;;;       |         |                ___
;;;       |  Upper  |              _|___|_
;;;   ____|_________|____         |___|___|
;;;  |         |         |      ___       ___
;;;  |  Left   |  Right  |    _|___|_   _|___|_
;;;  |_________|_________|   |___|___| |___|___|   ...
;;;

(defn outside-triangle? [x y h]
  "Checks if the coordinate [`x`, `y`] is outside the equilateral triangle with height h.
  Returns a boolean with the result."
  (cond (<  x h) (> y x)               ;; x in [0, h)  => f(x) = x
        (>= x h) (> y (- (* 2 h) x)))) ;; x in [h, 2h] => f(x) = 2h - x

(defn sierpinski [n h x y]
  "Takes the `n` number of iterations, triangle height `h` and coordinate [`x`, `y`].
  Performs `n` recursions over [`x`, `y`] transformations.
  Returns 1 if the coordinate belongs to sierpinski pattern or _ if not."
  (let [half-h (/ h 2)]
    (cond
      (outside-triangle? x y h) "_"

      (= n 0) "1"

      ;; (x, y) belongs to the upper block
      (> y half-h) (recur (dec n) half-h (- x half-h) (- y half-h))

      ;; (x, y) belongs to the right block
      (> x h)      (recur (dec n) half-h (- x h) y)

      ;; (x, y) belongs to left block
      :else        (recur (dec n) half-h x y))))

(defn print-sierpinski [n]
  "Prints the `n`th iteration of Sierpinski triangle in a grid of 32 by 63"
  (loop [y 32]
    (loop [x 63]
      (print (sierpinski n 32 x y))
      (when (> x 1) (recur (dec x))))
    (println)
    (when (> y 1) (recur (dec y)))))
