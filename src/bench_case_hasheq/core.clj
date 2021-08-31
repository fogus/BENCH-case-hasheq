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

(defn bench
  [{:keys [iterations] :as opts :or {iterations 1000000}}]
  (println "  Clojure version " *clojure-version*)

  (let [size-sm  10
        size-md  100
        size-lg  1000
        size-xl  10000
        size-xxl 100000]
    (let [data (->> (for [i (range size-xxl)] [i i]) (into {}))]
      (run-benchmark (str "TODO") (/ iterations 1)
                     (+ 1 2))))
)
