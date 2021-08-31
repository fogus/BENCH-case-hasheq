(ns bench-case-hasheq.core)

(set! *warn-on-reflection* true)

;;; BENCH STUFF

(def ^:dynamic *TIMES* 8)

(defn pretty-float5 [anum]
  (format "%.5g" anum))

(defn pretty-float3 [anum]
  (format "%.3g" anum))

(defn time-ms [amt afn]
  (let [start (System/nanoTime)
        _ (dotimes [_ amt] (afn))
        end (System/nanoTime)]
  (/ (- end start) 1000000.0)
  ))

(defn avg [numbers]
  (/ (reduce + numbers)
     (count numbers)
     1.0))

(defn average-time-ms [iters amt-per-iter afn]
  (avg
    ;; treat 1st run as warmup 
    (next
      (for [i (range (inc iters))]
        (time-ms amt-per-iter afn)))))

(defn update-vals-naive
  "m f => {k (f v) ...}

  Given a map m and a function f of 1-argument, returns a new map where the keys of m
  are mapped to result of applying f to the corresponding values of m."
  {:added "1.11"}
  [m f]
  (with-meta
    (zipmap (keys m) (map f (vals m)))
    (meta m)))

(defn compare-benchmark [amt-per-iter afn-map]
  (let [results (update-vals-naive
                 afn-map
                 (fn [afn]
                   (let [t  (average-time-ms *TIMES* amt-per-iter afn)]
                     {:t  t})))
        [[_ {best-time :t}] & _ :as sorted] (into (array-map) (sort-by (comp :t last) results))
        ]
    (println "\nAvg(ms)\t\tvs best\t\tCode")
    (doseq [[k {:keys [t sp]}] sorted]
      (println (pretty-float5 t) "\t\t" (pretty-float3 (/ t best-time 1.0)) "\t\t" k))))

(defmacro run-benchmark [name amt-per-iter & exprs]
  (let [afn-map (->> exprs shuffle (map (fn [e] [`(quote ~e) `(fn [] ~e)])) (into {}))]
    `(do
       (println "Benchmark:" ~name (str "(" (* *TIMES* ~amt-per-iter) " iterations with " ~amt-per-iter " warmup)"))
       (compare-benchmark ~amt-per-iter ~afn-map)
       (println "\n********************************\n"))))

(defn check-case-strings [arg]
  (case arg
    "foo" 1
    "bar" 2
    "baz" 3
    0))

(defn check-case-nums [arg]
  (case arg
    1 1
    2 2
    3 3
    0))

(defn check-case-keywords [arg]
  (case arg
    :foo 1
    :bar 2
    :baz 3
    0))

(defn check-case-tuple [arg]
  (case arg
    [:foo 1] 1
    [:bar 2] 2
    [:baz 3] 3
    0))

(defn bench
  [{:keys [iterations] :as opts :or {iterations 1000000}}]
  (println "  Clojure version " *clojure-version*)

  (let [size-sm  10
        size-md  100
        size-lg  1000
        size-xl  10000
        size-xxl 100000]
    (let [foo (constantly "foo")
          bar (constantly "bar")
          baz (constantly "baz")
          one (constantly 1)
          two (constantly 2)
          thr (constantly 3)
          kfoo (constantly :foo)
          kbar (constantly :bar)
          kbaz (constantly :baz)]
      (run-benchmark (str "case on") (/ iterations 1)
                     (+ (check-case-strings "foo")
                        (check-case-strings "bar")
                        (check-case-strings "baz")
                        (check-case-strings "quux"))

                     (+ (check-case-nums 1)
                        (check-case-nums 2)
                        (check-case-nums 3)
                        (check-case-nums 4))

                     (+ (check-case-keywords :foo)
                        (check-case-keywords :bar)
                        (check-case-keywords :baz)
                        (check-case-keywords :quux))

                     (+ (check-case-tuple [:foo 1])
                        (check-case-tuple [:bar 2])
                        (check-case-tuple [:baz 3])
                        (check-case-tuple [:quux 0])))))
)


(comment

  (case [-1] [-1] true false)

  (case [">" (compare 2 3)]
    [">" -1] true
    false)

  (let [ary (int-array [-5 -4 -3 -2 -1 0 1 2 3 4 5])]
    (case [(aget ary (rand-int 12))]
      [(int -1)] true
      false))
)
