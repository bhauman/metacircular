(ns metacircular.core)

(enable-console-print!)

(defn function? [x]
  (goog/isFunction x))

(defn cont-trampoline
  ([f]
   (if (and (fn? f) (get (meta f) :continuation))
     (recur (f))
     f))
  ([f & args]
   (cont-trampoline (apply f args))))

(declare eval-k eval-fn eval-seq eval-do eval-breakpoint)

(defonce debugger-state (atom {}))

(def initial-env-map
  (atom { 'println    (atom println)
          '+          (atom +)
          'do         (atom (list 'syntax-primitive eval-do))
          'fn         (atom (list 'syntax-primitive eval-fn))
          'breakpoint (atom (list 'syntax-primitive eval-breakpoint)) }))

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

(defn eval-fn [exp env k]
  (let [arg-decl (first (rest exp))]
    (k
     (with-meta
       (fn [args k']
         (eval-seq
          (rest (rest exp))
          ;; this is not a do exp should be
          (env-extend* env arg-decl args)
          k'))
       {:lambda true}))))

(defn enter-stepper [exp env k]
  (swap! debugger-state
         assoc
         :exp exp
         :env env
         :continuation k)
  (prn exp))

(defn leave-stepper []
  (println "leaving stepper")
  (swap! debugger-state dissoc :stepping))

(defn eval-breakpoint [exp env k]
  (enter-stepper exp env k)
  (println "Entering debugger"))

(defn continue! []
  (leave-stepper)
  (cont-trampoline ((:continuation @debugger-state) nil)))

(defn p! [v]
  {:pre [(var? v)]}
  (env-lookup (:env @debugger-state) v))

(defn step! []
  (swap! debugger-state assoc :stepping true)
  (cont-trampoline ((:continuation @debugger-state))))

;; define your app data so that it doesn't get over-written on reload

;; no need for closures this is much faster
;; but may run into problems in javascript implementation
;; just never use env directly and you are good for a reimplementation

(defn app? [x] (and (list? x) (>= (count x) 1 )))
(def app->fun first)
(def app->args rest)

(declare perform-apply-k)

(defn continuation [k]
  (fn [x]
    (with-meta (fn [] (k x)) {:continuation true})))

(defn eval-k [exp env k]
  (let [k (continuation k)
        next-step
        (cond
          (symbol? exp) (k (env-lookup env exp))
          (string? exp) (k exp)
          (number? exp) (k exp)
          ;; used for boxing 3d constructs and atom would work as well
          (function? exp) (k (exp))
          (app? exp)
          (eval-k (app->fun exp)
                  env
                  (fn [res]
                    (perform-apply-k res
                                     exp env k)))
          :else
          (throw "RE: Unkown expression type"))]
    (if-not (:continuation (meta next-step))
      (do
        (leave-stepper)
        next-step)
      (if (:stepping @debugger-state)
        (enter-stepper exp env next-step)
        next-step))))

(defn eval [exp env]
  (cont-trampoline eval-k exp env identity))

(defn syntax-primitive? [x] (d-tagged-list? 'syntax-primitive x))
(def syntax-primitive->eval (comp first rest))

(defn macro? [x] (d-tagged-list? 'macro x))
(def macro->proc (comp first rest))

(defn lambda? [x] (get (meta x) :lambda))

(defn eval-args [args env k]
  (if (empty? args)
    (k '())
    (eval-k (first args)
            env
            (fn [a]
              (eval-args (rest args) env (fn [x] (k (cons a x))))))))

(defn eval-seq [exps env k]
  (eval-args exps env (fn [vals'] (k (last vals')))))

(defn eval-do [exp env k]
  (eval-seq (rest exp) env k))

(defn perform-apply-k [fun app-exp env k]
  (prn "apply" (if (seq? fun) (first fun) nil))
  (let [args (app->args app-exp)]
    (cond
      (syntax-primitive? fun) ((syntax-primitive->eval fun)
                               app-exp env k)
      (macro? fun) (eval-k (apply (macro->proc fun) args) env k)
      (lambda? fun) (apply fun [args k])
      :else (eval-args args env (fn [args']
                                  (k (apply fun args')))))))


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
  
  
  (eval '((fn [x y] (+ x y)) 5 7) initial-env-map)  

  (eval '(do 1 2 3 (+ 1 2 (+ 7 8))) initial-env-map)
  (eval '(do 1) initial-env-map)

  (eval '((fn [x y] 4 (+ x y)) 5 7) initial-env-map)

  (eval '((fn [x y] 4 (breakpoint) (+ x y)) 5 7) initial-env-map)
  
  ;; syntax primitive macro
  (env-set! initial-env-map
            'by-five
            (list 'macro (fn [x] (list '+ x x x x x))))

  ;; first class macro
  
  (eval '(+ 1 (by-five (+ 1 3))) initial-env-map)
  
  )
