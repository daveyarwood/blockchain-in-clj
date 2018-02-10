(ns blockchain
  (:require [blockchain.block   :as b]
            [clojure.spec.alpha :as s]))

(defn -main []
  (let [block1 (b/block "boop" "0")
        block2 (b/block "coop" (::b/hash block1))
        block3 (b/block "doop" (::b/hash block2))]
    (s/explain ::b/blockchain [block1 block2 block3])
    (s/explain ::b/proven-blockchain
               [5 (map #(b/mine % 5) [block1 block2 block3])])))
