(local std (include :std))

(fn deftest
  [name ...]
  (each [_ test (ipairs [...])]
    (when (not test)
      (print "FAIL: " name)
      (lua "return")))
  (print "Success" name))

(deftest
 "map"
 (= (. (std.map (fn inc [x] (+ 1 x)) [1]) 1) 2))

(deftest
 "apply"
 (= (std.apply math.min [1 2 3]) 1))

(deftest
 "equal?"
 (std.equal? 1 1)
 (not (std.equal? 1 2))
 (std.equal? [1 2] [1 2])
 (not (std.equal? [1 2] [2 1]))
 (not (std.equal? [1 2] [1 2 3])))

(deftest
 "range"
  (std.equal? (std.range 3) [1 2 3])
  (std.equal? (std.range 3 1 -1) [3 2 1])
  (std.equal? (std.range 2 10 2) [2 4 6 8 10]))
