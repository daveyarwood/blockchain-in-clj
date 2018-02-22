(ns blockchain
  (:require [blockchain.block         :as    b]
            [blockchain.coin          :as    c]
            [blockchain.transaction   :as    tx]
            [blockchain.util.encoding :refer (base-64)]
            [clojure.pprint           :refer (pprint)]
            [clojure.spec.alpha       :as    s]
            [clojure.spec.test.alpha  :as    stest]))

(defn- message
  [x msg]
  (println msg)
  x)

(defn- check-validity
  [cryptocurrency]
  (s/explain ::c/cryptocurrency cryptocurrency)
  cryptocurrency)

(defn -main []
  (stest/instrument)
  (let [n00bcoin (c/cryptocurrency 0.1)
        coinbase (tx/generate-keypair)
        kp-1     (tx/generate-keypair)
        kp-1-pub (.getPublicKey kp-1)
        kp-2     (tx/generate-keypair)
        kp-2-pub (.getPublicKey kp-2)]
    (-> n00bcoin
        (message "Checking validity of fresh cryptocurrency...")
        check-validity
        (message "Coinbase sending 100 coins to person 1...")
        (c/send-funds coinbase kp-1-pub 100 {::c/genesis? true})
        check-validity
        ;; this appropriately fails spec because of insufficient funds
        ;; (c/send-funds kp-1 kp-2-pub 1000 {})
        (message "Person 1 sending 40 coins to person 2...")
        (c/send-funds kp-1 kp-2-pub 40 {})
        check-validity
        (message "Person 2 sending 20 coins to person 1...")
        (c/send-funds kp-2 kp-1-pub 20 {})
        check-validity
        (message "Pretty-printed blockchain:")
        ::b/blockchain
        (doto pprint)
        (message "Checking validity of blockchain...")
        (doto (#(s/explain ::b/blockchain %)))
        (#(map (fn [block] (println "Mining block...") (b/mine block 4)) %))
        (message "Checking validity of proven blockchain...")
        (#(s/explain ::b/proven-blockchain [4 %])))))
