(fn cond
  [expr1 body1 ...]
  ;; (assert (and expr1 body1) "std.cond expects at least 2 forms")
  (if (= (length [...]) 0)
      `(if ,expr1 ,body1 (error "No matching std.cond clause"))
      `(if ,expr1 ,body1
           (cond ,...))))

(fn bindings->symvals
  [bindings]
  (let [syms []
        vals []]
    (for [i 1 (length bindings) 2]
      (table.insert syms (. bindings i))
      (table.insert vals (. bindings (+ i 1))))
    [syms vals]))

(fn let*
  [name bindings body1 ...]
  (let [[syms vals] (bindings->symvals bindings)]
    `(do
       (fn ,name [,(unpack syms)] ,body1 ,...)
       (,name ,(unpack vals)))))

{:cond cond
 :let* let*}
