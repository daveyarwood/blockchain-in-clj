(ns blockchain
  (:require [blockchain.block          :as b]
            [blockchain.transaction    :as tx]
            [blockchain.util.encoding  :refer (base-64)]
            [clojure.spec.alpha        :as s]))

(comment
  (let [block1 (b/block "boop" "0")
        block2 (b/block "coop" (::b/hash block1))
        block3 (b/block "doop" (::b/hash block2))]
    (s/explain ::b/blockchain [block1 block2 block3])
    (s/explain ::b/proven-blockchain
               [5 (map #(b/mine % 5) [block1 block2 block3])])))

(defn -main []
  (let [kp-1          (tx/generate-keypair)
        kp-2          (tx/generate-keypair)
        public-key-1  (.getPublicKey  kp-1)
        private-key-1 (.getPrivateKey kp-1)
        public-key-2  (.getPublicKey  kp-2)
        private-key-2 (.getPrivateKey kp-2)
        transaction   (tx/transaction public-key-1 public-key-2 5 [nil])]
    (s/explain ::tx/transaction transaction)
    (let [signature (tx/sign transaction private-key-1)]
      (prn (tx/verify-signature transaction signature public-key-1)))))
