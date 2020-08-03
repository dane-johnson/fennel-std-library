(local std {})

(import-macros {: cond} :std-macros)

(lambda std.not
  [x]
  "Returns true iff x is falsey"
  (not x))

(lambda std.empty?
  [s]
  "Returns true if s is an empty table"
  (= (length s) 0))

(lambda std.comp
  [f ...]
  "Combines args into a one function that apples each function in turn"
  (let [args [...]]
    (if (std.empty? args)
        f
        (fn [x] (f ((std.comp (unpack args)) x))))))

(lambda std.take
  [n s]
  "Get the first n values of a sequence as a new array."
  (let [arr []]
    (for [i 1 n]
         (table.insert arr (. s i)))
    arr))

(lambda std.take-while
  [p s]
  "Gets values from the sequence until p is falsey"
  (var i 1)
  (while (and (<= i (length s)) (p (. s i)))
    (set i (+ i 1)))
  (std.take (- i 1) s))

(lambda std.drop
  [n s]
  "Get s with the first n values removed as a new array."
  (let [arr []]
    (for [i (+ n 1) (length s)]
         (table.insert arr (. s i)))
    arr))

(lambda std.drop-while
  [p s]
  "Get s with the first sequence of (p v) values removed." ;; TODO fix this horrible docstring
  (var i 1)
  (while (and (<= i (length s)) (p (. s i)))
    (set i (+ i 1)))
  (std.drop (- i 1) s))

(lambda std.first
  [s]
  "Gets the first value of the sequence."
  (. s 1))

(lambda std.second
  [s]
  "Gets the second value of the sequence."
  (. s 2))

(lambda std.third
  [s]
  "Gets the third value of the sequence."
  (. s 3))

(lambda std.fourth
  [s]
  "Gets the fourth value of the sequence."
  (. s 4))

(lambda std.rest
  [s]
  "Gets all but the first value of the sequence as a new array."
  (std.drop 1 s))

(lambda std.last
  [s]
  "Gets the last value of the sequence."
  (. s (length s)))

(lambda std.butlast
  [s]
  "Gets all but the last value of the sequence as a new array."
  (std.take (- (length s) 1) s))

(lambda std.apply
  [f ...]
  "Calls f with args, unpacking the last argument."
  (if (= (length [...]) 1)
      (f (unpack (std.first [...])))
      (f (unpack (std.butlast [...])) (unpack (std.last [...])))))

(lambda std.cons
  [v s]
  "Returns a fresh table with v at the front of s"
  (let [tbl [v]]
    (each [_ v (ipairs s)]
      (table.insert tbl v))
    tbl))

(lambda std.every?
  [p s]
  "Returns true if (p v) is truthy for all v in s"
  (cond
   (std.empty? s) true
   (p (std.first s)) (std.every? p (std.rest s))
   :else false))

(lambda std.foldr
  [f k s]
  (if (std.empty? s)
      k
      (f (std.first s) (std.foldr f k (std.rest s)))))

(lambda std.foldl
  [f k s]
  (if (std.empty? s)
      k
      (std.foldl f (f k (std.first s)) (std.rest s))))

(lambda std.interleave
  [...]
  "Returns the first value of each sequence, then the second, etc. up to the length of the shortest sequence"
  (if (std.every? (std.comp std.not std.empty?) [...])
      (std.cons (std.map std.first [...])
                (std.apply std.interleave (std.map std.rest [...])))
      []))

(lambda std.map
  [f s ...]
  "Returns the result of applying f to each element in s as a new table."
  (let [tbl []]
    (if (std.empty? [...])
        ;; This avoids mutual recursion
        (each [_ v (ipairs s)]
          (table.insert tbl (f v)))
        (each [_ vs (ipairs (std.interleave s ...))]
          (table.insert tbl (std.apply f vs))))
    tbl))

(lambda std.foreach
  [f s ...]
  "Calls f on each element of s, presumably for side effects. Returns nil."
  (each [_ vs (ipairs (std.interleave s ...))]
        (std.apply f vs))
  nil)

(lambda std.kvs
  [t]
  (let* loop [tbl []
              k (next t)]
    (if k
        (loop (std.cons tbl [k (. t k)]) (next t k))
        tbl)))

(lambda std.some
  [p s]
  "Returns the first value of s where (p v) is truthy, or nil if none are."
  (var tval nil)
  (var i 1)
  (while (and (not tval) (<= i (length s)))
    (when (p (. s i))
      (set tval (. s i)))
    (set i (+ i 1)))
  tval)

(lambda std.keys
  [t]
  (std.map std.first (std.kvs t)))

(lambda std.vals
  [t]
  (std.map std.second (std.kvs t)))

;; Wrap the special forms TODO

;; (lambda std.=
;;   [x y ...]
;;   "Returns true iff all arguments are equal (in the Lua sense, table equality is by reference!)"
;;   (std.every? #))

;; (lambda std.not=
;;   [x y ...]
;;   "Returns true iff any argument are not equal (in the Lua sense, table equality is by reference!)"
;;   (not= x y ...))

;; (lambda std.<
;;   [x y ...]
;;   "Returns true if x < y < ..."
;;   (< x y ...))

;; (lambda std.>
;;   [x y ...]
;;   "Returns true if x > y > ..."
;;   (> x y ...))

;; (lambda std.<=
;;   [x y ...]
;;   "Returns true if x <= y <= ..."
;;   (<= x y ...))

;; (lambda std.>=
;;   [x y ...]
;;   "Returns true if x >= y >= ..."
;;   (>= x y ...))

;; (lambda std.+
;;   [...]
;;   "Returns the sum of it's arguments"
;;   (+ ...))

;; (lambda std.-
;;   [x ...]
;;   "If used in unary form, returns the opposite of it's arguement. Otherwise returns the difference of its arguments."
;;   (- x ...))

;; (lambda std.*
;;   [...]
;;   "Returns the product of its arguments."
;;   (* ...))

;; (lambda std./
;;   [x ...]
;;   "Returns the quotient of its arguments"
;;   (/ x ...))

;; (lambda std.//
;;   [x ...]
;;   "Returns the integral part of the quotient of its arguments"
;;   (// x ...))

;; (lambda std.%
;;   [x ...]
;;   "Returns the remainder of its arguments (NOT the modulus)"
;;   (% x ...))

;; (lambda std.^
;;   [x ...]
;;   "Returns the result of x^...^1 where ^ is the exponential function"
;;   (^ x ...))

std
