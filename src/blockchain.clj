(ns blockchain
  (:require [clojure.spec.alpha :as s]
            [digest             :refer (sha-256)]))

(s/def ::hash string?)
(s/def ::previous-hash ::hash)
(s/def ::data string?)
(s/def ::timestamp pos-int?)
(s/def ::block (s/keys :req [::hash ::previous-hash ::data ::timestamp]))

(defn calculate-hash
  [{::keys [previous-hash timestamp data] :as block}]
  (sha-256 (str previous-hash timestamp data)))

(s/def ::blockchain
  (s/and (s/coll-of ::block)
         sequential?
         #(every? (fn [{::keys [hash] :as block}]
                    (= hash (calculate-hash block)))
                  %)
         #(every? (fn [[block-a block-b]]
                    (= (::hash block-a) (::previous-hash block-b)))
                  (partition 2 1 %))))

(defn block
  [data previous-hash]
  (let [block'    {::previous-hash previous-hash
                   ::data          data
                   ::timestamp     (System/currentTimeMillis)}
        timestamp (System/currentTimeMillis)]
    (assoc block' ::hash (calculate-hash block'))))

(defn -main []
  (let [block1 (block "boop" "0")
        block2 (block "coop" (::hash block1))
        block3 (block "doop" (::hash block2))]
    (s/explain ::blockchain [block1 block2 block3])))
