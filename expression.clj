(load-file "proto.clj")
; math
(defn div
  ([x] (/ 1 (double x)))
  ([x & args] (/ (double x) (apply * args))))
(defn sq [x] (* x x))
(defn mean_ [& args] (div (apply + args) (count args)))
(defn neg [x] (- x))

; functional
(defn func [op]
  (fn [& args]
    (fn [vars]
      (apply op (mapv #(% vars) args)))))

(def constant constantly)

(defn variable [name]
  #(get % name))

(def add (func +))
(def subtract (func -))
(def multiply (func *))
(def divide (func div))
(def mean (func mean_))
(def varn (func (fn [& args] (let [m (apply mean_ args)] (apply mean_ (mapv #(sq (- % m)) args))))))
(def negate (func neg))

(def functional-ops {
                     '+ add
                     '- subtract
                     '* multiply
                     '/ divide
                     'mean mean
                     'varn varn
                     'negate negate
                     })

; object

(defn init [type_ names]
  (mapv #(eval `(def ~(symbol %) (~type_ ~(keyword %)))) names))

(init method ["evaluate", "toString", "apply_", "diff-impl", "diff"])
(init field ["args", "signs", "val_"])

(defn make-cv [evaluate__ toString__ diff__]
  (letfn [(f [this val__]
            (assoc this
              :val_ val__))]
    (constructor f
                 {:evaluate evaluate__
                  :toString toString__
                  :diff diff__})))

(declare ZERO)
(def Constant
  (make-cv
    (fn [this _] (val_ this))
    (fn [this] (str (val_ this)))
    (fn [_ _] ZERO)))

(def ZERO (Constant 0))
(def ONE (Constant 1))

(def Variable
  (make-cv
    (fn [this vars] (get vars (val_ this)))
    (fn [this] (val_ this))
    (fn [this n] (if (= n (val_ this)) ONE ZERO))))

(def op-proto {
               :evaluate (fn [this vars] (apply apply_ this (mapv #(evaluate % vars) (args this))))
               :toString (fn [this] (str "(" (first (signs this)) " " (clojure.string/join " " (mapv toString (args this))) ")"))
               :diff (fn [this name] (diff-impl this (args this) (mapv #(diff % name) (args this))))
               })

(defn make-op [signs__ apply__ diff-impl__]
  (letfn [(op [this & args__]
            (assoc this
              :args (vec args__)))]
    (constructor op
                 (assoc op-proto
                   :apply_ (fn [this & args__] (apply apply__ args__))
                   :signs signs__
                   :diff-impl diff-impl__))))

(def Add (make-op
           ["+"]
           +
           (fn [_ _ diffs] (apply Add diffs))))
(def Subtract (make-op
                ["-"]
                -
                (fn [_ _ diffs] (apply Subtract diffs))))
(def Multiply (make-op
                ["*"]
                *
                (fn [_ args diffs]
                  (apply Add (mapv #(apply Multiply (assoc args % (nth diffs %)))
                                   (range (count args)))))))
(defn square [x]
  (Multiply x x))

(def Negate (make-op
              ["negate"]
              neg
              (fn [_ _ diffs] (apply Negate diffs))))

(def Divide (make-op
              ["/"]
              div
              (fn [_ args diffs]
                (if (= 1 (count args))
                  (Negate (Divide (first diffs) (square (first args))))
                  (apply Subtract
                         (Divide
                           (first diffs)
                           (apply Multiply (rest args)))
                         (mapv #(apply Divide
                                       (Multiply (first args) (diffs %))
                                       (assoc (vec (rest args)) (- % 1) (square (args %))))
                               (range 1 (count args))))))))

(def ArithMean (make-op
                 ["arith-mean"]
                 mean_
                 (fn [_ _ diffs]
                   (apply ArithMean diffs))))

(def Sign (make-op
            ["sign"]
            (fn [& args] (if (>= (first args) 0) 1 (- 1)))
            #()))

(defn power [x pow]
  (if (== pow 0)
    ONE
    (apply Multiply (repeat pow x))))

(def GeomMean (make-op
                ["geom-mean"]
                (fn [& args]
                  (Math/pow (Math/abs^double (apply * args)) (/ (count args))))
                (fn [this, args, diffs]
                  (Divide
                    (apply ArithMean
                           (mapv #(apply Multiply (assoc args % (nth diffs %)))
                                 (range (count args))))
                    (Multiply
                      (Sign (apply Multiply args))
                      (power this (- (count args) 1)))))))

(def HarmMean (make-op
                ["harm-mean"]
                (fn [& args] (div (count args) (apply + (mapv #(div %) args))))
                (fn [this args diffs]
                  (Multiply
                    (apply Add (mapv #(Divide (diffs %) (square (args %))) (range (count args))))
                    (Divide (square this) (Constant (count args)))))))

(def object-ops {
                 '+ Add
                 '- Subtract
                 '* Multiply
                 '/ Divide
                 'negate Negate
                 'arith-mean ArithMean
                 'geom-mean GeomMean
                 'harm-mean HarmMean
                 })

; parser
(defn make-parser [ops const_ var_]
  (fn parse [expr]
    (cond
      (number? expr) (const_ expr)
      (symbol? expr) (var_ (str expr))
      (list? expr) (apply (get ops (first expr))
                          (mapv parse (rest expr))))))

(defn parseSmth [& context]
  (fn [str]
    ((apply make-parser context) (read-string str))))

(def parseFunction
  (parseSmth functional-ops constant variable))

(def parseObject
  (parseSmth object-ops Constant Variable))