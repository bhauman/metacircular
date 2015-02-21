(ns ^:figwheel-always metacircular.core
    (:require))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

(defn function? [x]
  (js* "typeof(x) === 'function';"))

(declare eval eval-fn eval-unquote)

(def initial-env-map
  (atom { 'println (atom println)
          '+       (atom +)
          'fn      (atom (list 'syntax-primitive eval-fn))
   }))

(def var? symbol?)

(defn env-extend [env var val]
  { :pre [(var? var)]
    :post [(map? %)]}
  (atom (assoc @env var (atom val))))

(defn env-extend* [env vars vals]
  (atom
   (merge @env
          (into {} (map (fn [v vl] [v (atom vl)]) vars vals)))))

(defn env-lookup [env var]
  {:pre [(var? var)]}
  (if-let [val (get @env var)]
    @val
    (throw (str "RE: Unkown variable " var))))

(defn env-set!
  { :pre [(var? var)]}
  [env var val]
  (if-let [v (get env var)]
    (reset! v val)
    (swap! env assoc var (atom val))))

(defn initial [sym]
  (env-lookup initial-env-map sym))

(defn tagged-list? [tag xs]
  (and (seq? xs)
       (= (first xs) tag)))

(defn d-tagged-list? [tag xs]
  (or (tagged-list? tag xs)
      (and (seq? xs)
           (function? (first xs))
           (= ((first xs)) (initial tag)))))

(defn eval-fn [exp env]
  (let [arg-decl (first (rest exp))]
    (fn [& args]
      (eval
       (first (rest (rest exp)))
       ;; this is not a do exp should be
       (env-extend* env arg-decl args)))))

(defn eval-unquote [exp env]
  (eval (first (rest exp))
        env))

;; define your app data so that it doesn't get over-written on reload

;; no need for closures this is much faster
;; but may run into problems in javascript implementation
;; just never use env directly and you are good for a reimplementation

(defn app? [x] (and (list? x) (>= (count x) 1 )))
(def app->fun first)
(def app->args rest)

(declare perform-apply)

(defn eval [exp env]
  (prn exp)
  (cond
    (symbol? exp) (env-lookup env exp)
    (string? exp) exp
    (number? exp) exp
    ;; used for boxing 3d constructs and atom would work as well
    (function? exp) (exp)
    (app? exp)
    (perform-apply (eval (app->fun exp) env)
                     exp env)
    :else
    (throw "RE: Unkown expression type")))

(defn syntax-primitive? [x] (d-tagged-list? 'syntax-primitive x))
(def syntax-primitive->eval (comp first rest))

(defn macro? [x] (d-tagged-list? 'macro x))
(def macro->proc (comp first rest))

(defn perform-apply [fun app-exp env]
  (prn "apply" fun)
  (let [args (app->args app-exp)]
    (cond
      (syntax-primitive? fun) ((syntax-primitive->eval fun)
                               app-exp env)
      (macro? fun) (eval (apply (macro->proc fun) args) env)
      :else (apply fun (map #(eval % env) args)))))




(comment
  (def x (atom {'a (atom 5)}))
  
  (env-set! x 'a 6)
  (env-lookup x 'b)
  (env-lookup x 'a)  
  
  (eval 'a (atom {'a (atom 5)}))
  (eval "hi" (atom {}))
  (eval 2 (atom {}))
  
  (eval (fn [] "hey") (atom {}))

  (eval '(println "hello world") initial-env-map)
  (eval '(+ 1 2 3 (+ 4 5)) initial-env-map)
  (eval '((fn [x] x) 5) initial-env-map)

  ;; syntax primitive macro
  (env-set! initial-env-map
            'by-five
            (list 'macro (fn [x] (list '+ x x x x x))))

  ;; first class macro
  
  (eval '(+ 1 (by-five (+ 1 3))) initial-env-map)
  
  )

