(ns blockchain.block
  (:require [blockchain.transaction   :as    tx]
            [blockchain.util.encoding :refer (sha-256 merkle-root)]
            [clojure.spec.alpha       :as    s]
            [clojure.string           :as    str]))

(s/def ::hash string?)
(s/def ::previous-hash ::hash)
(s/def ::transactions (s/and (s/coll-of ::tx/transaction) sequential?))
(s/def ::timestamp pos-int?)
(s/def ::nonce int?)
(s/def ::proof (s/keys :req [::hash ::nonce]))

(defn calculate-hash
  [{::keys [previous-hash timestamp transactions nonce] :as block}]
  (->> transactions
       (map ::tx/hash)
       merkle-root
       (str previous-hash timestamp nonce)
       sha-256))

(defn correct-hash?
  [{::keys [hash] :as block}]
  (= hash (calculate-hash block)))

(s/def ::block
  (s/and
    (s/keys :req [::hash ::previous-hash ::transactions ::timestamp]
            :opt [::proof])
    correct-hash?
    #(or (not (::proof %))
         (correct-hash? (merge (::proof %) %)))))

(defn block
  "Returns an unmined block (nonce = 0) with the provided transactions and
   previous hash."
  [transactions previous-hash]
  (let [timestamp (System/currentTimeMillis)
        block'    {::previous-hash previous-hash
                   ::transactions  transactions
                   ::timestamp     timestamp
                   ::nonce         0}]
    (assoc block' ::hash (calculate-hash block'))))

(defn genesis-block
  []
  (block [] "0"))

(s/def ::blockchain
  (s/and (s/coll-of ::block)
         sequential?
         #(every? (fn [[block-a block-b]]
                    (= (::hash block-a) (::previous-hash block-b)))
                  (partition 2 1 %))))

(s/def ::difficulty int?)

(defn meets-difficulty?
  "Returns true if the block's proof hash proves work up to the given
   difficulty, i.e.  has enough leading zeros."
  [{::keys [hash]} difficulty]
  (->> \0
       (repeat difficulty)
       (apply str)
       (str/starts-with? hash)))

(s/def ::proven-blockchain
  (s/and (s/cat :difficulty ::difficulty :blockchain ::blockchain)
         (fn [{:keys [difficulty blockchain]}]
           (every? #(meets-difficulty? (::proof %) difficulty) blockchain))))

(defn chip
  "Returns an iteration of mining a block, wherein ::nonce is incremented and
   ::hash is recomputed."
  [block]
  (as-> block ?
    (update ? ::nonce inc)
    (assoc ? ::hash (calculate-hash ?))))

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

