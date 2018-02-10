(ns blockchain
  (:require [clojure.spec.alpha :as    s]
            [clojure.string     :as    str]
            [digest             :refer (sha-256)]))

;; library

(defn calculate-hash
  [{::keys [previous-hash timestamp data nonce] :as block}]
  (sha-256 (str previous-hash timestamp nonce data)))

(defn correct-hash?
  [{::keys [hash] :as block}]
  (= hash (calculate-hash block)))

(defn block
  "Returns an unmined block (nonce = 0) with the provided data and previous
   hash."
  [data previous-hash]
  (let [timestamp (System/currentTimeMillis)
        block'    {::previous-hash previous-hash
                   ::data          data
                   ::timestamp     timestamp
                   ::nonce         0}]
    (assoc block' ::hash (calculate-hash block'))))

(defn chip
  "Returns an iteration of mining a block, wherein ::nonce is incremented and
   ::hash is recomputed."
  [block]
  (as-> block ?
    (update ? ::nonce inc)
    (assoc ? ::hash (calculate-hash ?))))

(defn meets-difficulty?
  "Returns true if the block's proof hash proves work up to the given
   difficulty, i.e.  has enough leading zeros."
  [{::keys [hash]} difficulty]
  (->> \0
       (repeat difficulty)
       (apply str)
       (str/starts-with? hash)))

(defn mine
  "Tries re-hashing with different nonce values in succession until the hash has
   enough leading zeros to satisfy the mining difficulty.

   Returns the block with an added ::proof key, containing the adjusted ::nonce
   and ::hash values."
  [block difficulty]
  (as-> block ?
    (iterate chip ?)
    (drop-while #(not (meets-difficulty? % difficulty)) ?)
    (first ?)
    (select-keys ? [::hash ::nonce])
    (assoc block ::proof ?)))

;; spec

(s/def ::hash string?)
(s/def ::previous-hash ::hash)
(s/def ::data string?)
(s/def ::timestamp pos-int?)
(s/def ::nonce int?)
(s/def ::proof (s/keys :req [::hash ::nonce]))

(s/def ::block (s/and
                 (s/keys :req [::hash ::previous-hash ::data ::timestamp]
                         :opt [::proof])
                 correct-hash?
                 #(or (not (::proof %))
                      (correct-hash? (merge (::proof %) %)))))

(s/def ::blockchain
  (s/and (s/coll-of ::block)
         sequential?
         #(every? (fn [[block-a block-b]]
                    (= (::hash block-a) (::previous-hash block-b)))
                  (partition 2 1 %))))

(s/def ::difficulty int?)

(s/def ::proven-blockchain
  (s/and (s/cat :difficulty ::difficulty :blockchain ::blockchain)
         (fn [{:keys [difficulty blockchain]}]
           (every? #(meets-difficulty? (::proof %) difficulty) blockchain))))

;; main

(defn -main []
  (let [block1 (block "boop" "0")
        block2 (block "coop" (::hash block1))
        block3 (block "doop" (::hash block2))]
    (s/explain ::blockchain [block1 block2 block3])
    (s/explain ::proven-blockchain
               [5 (map #(mine % 5) [block1 block2 block3])])))
