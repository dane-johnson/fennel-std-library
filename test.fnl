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
