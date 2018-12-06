(ns main)

(defn Or [x]
  (if (some true? x)
    true
    (let [i (remove false? (distinct (rest x)))]            ;remove false, duplicate symbols, and ignore "or"
      (cond
        (= (count i) 0) false                               ;empty list means false
        (= (count i) 1) (first i)
        (> (count i) 1) (remove false? x)                   ;more than 1 variable, return with "or"
        )
      )
    )
  )

(defn And [x]
  (if (some false? x)
    false
    (let [i (remove true? (distinct (rest x)))]             ;remove true, duplicate symbols, and ignore "and"
      (cond
        (= (count i) 0) true                                ;empty list means true
        (= (count i) 1) (first i)
        (> (count i) 1) (remove true? x)                    ;more than variable, return with "and"
        )
      )
    )
  )

(defn Not [x]                                               ;only true or false can be evaluated
  (cond
    (some true? (rest x)) false
    (some false? (rest x)) true
    :default x                                              ;if variable return input
    )
  )

(defn decideOperator [x]                                    ;chooses Or, And, or Not functions
  (cond
    (some {'or true} x) (Or x)
    (some {'and true} x) (And x)
    (some {'not true} x) (Not x)
    )
  )

(defn simplify [x]
  (decideOperator
    (distinct                                               ;remove excess symbols ex) (or x y (or x false)) -> (or x y)
      (map
        (fn [i]
          (if (seq? i)                                      ;if element is seq
            (if (some seq? i)                               ;  if element contains a list
              (simplify i)                                  ;    recursive call
              (decideOperator i)                            ;    else element is a seq that doesn't contain another seq -> can evaluate, call decideOperator
              )
            i)                                              ;  return element if it is not a seq
          )
        x)
      )
    )
  )

(defn bind-values [m l]
  (map
    (fn [i]
      (if (seq? i)                                          ;if element is seq
        (bind-values m i)                                   ;  recursive call
        (get m i i)                                         ;  else replace element with it's binding, if no binding return original element
        )
      )
    l)
  )

(defn evalexp [exp bindings]
  (simplify
    (bind-values bindings exp)
    )
  )