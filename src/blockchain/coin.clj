(ns blockchain.coin
  (:require [blockchain.block       :as b]
            [blockchain.transaction :as tx]
            [clojure.spec.alpha     :as s])
  (:import [org.bouncycastle.openpgp PGPKeyPair PGPPublicKey]))

(s/def ::minimum-transaction (s/and number? pos?))

;; UTXO is bitcoin jargon for Unspent Transaction Outputs. Your current balance
;; is the sum of all UTXOs addressed to you.
;;
;; If a blockchain is very, very long, it could take a really long time to
;; process a new transaction because we have to find and check all of its
;; inputs. To get around this, we can keep a cache of all UTXOs that can be used
;; as inputs.
(s/def ::utxos (s/map-of ::tx/transaction-id ::tx/output))

(s/def ::cryptocurrency
  (s/keys :req [::b/blockchain
                ::utxos
                ::minimum-transaction]))

(defn cryptocurrency
  "Returns a fresh new cryptocurrency, with only a genesis block and no UTXOs."
  [minimum-transaction]
  {::b/blockchain        [(b/genesis-block)]
   ::utxos               {}
   ::minimum-transaction minimum-transaction})

(defn balance
  "Returns the balance of the owner of a public key."
  [{::keys [utxos] :as cryptocurrency} public-key]
  (->> utxos
       (filter (fn [{::tx/keys [recipient]}] (= public-key recipient)))
       (apply +)))

(defn gather-funds
  "Gathers just enough of a sender's UTXOs to meet or exceed the desired amount.
   Order is non-deterministic, as the UTXOs are stored in a map.

   If the sender's balance is not >= the desired amount, then all of the
   sender's UTXOs are returned, as if to say, \"here, this is all I've got.\".
   Calling code needs to handle this scenario."
  [{::keys [utxos] :as cryptocurrency} public-key desired-amount]
  (-> (reduce (fn [[ids total] [id {::tx/keys [recipient value] :as tx}]]
                (cond
                  (>= total desired-amount) (reduced [ids total])
                  (= public-key recipient)  [(conj ids id) (+ total value)]
                  :else                     [ids total]))
              [[] 0]
              utxos)
      first))

(s/def ::tx/error
  #{::tx/amount-too-small ::tx/insufficient-funds})

(s/def ::tx/options
  (s/keys :opt [::genesis?]))

(s/fdef transact
  :args (s/cat :cryptocurrency ::cryptocurrency
               :transaction    ::tx/transaction
               :opts           ::tx/options)
  :ret  (s/or ::crytocurrency ::tx/error))

(defn transact
  [{::keys [utxos minimum-transaction] :as cryptocurrency}
   {::tx/keys [hash inputs value sender recipient] :as transaction}
   {::keys [genesis?] :as opts}]
  (let [input-balance (->> (for [tx-id inputs
                                 :let [output (get utxos tx-id)]
                                 :when output]
                             (::tx/value output))
                           (reduce +))]
    (cond
      (and (not genesis?) (< input-balance minimum-transaction))
      ::tx/amount-too-small

      (and (not genesis?) (< input-balance value))
      ::tx/insufficient-funds

      :else
      (let [;; the recipient gets the amount of the transaction
            output-1 (tx/output hash recipient value)
            ;; the sender gets the "change" back (inputs - value)
            output-2 (when-not genesis?
                       (tx/output hash sender (- input-balance value)))]
        (-> cryptocurrency
            (update ::utxos
                    #(as-> % ?
                       (assoc ? (::tx/hash output-1) output-1)
                       (if output-2
                         (assoc ? (::tx/hash output-2) output-2)
                         ?)
                       (apply dissoc ? inputs)))
            (update ::b/blockchain
                    conj
                    (let [{::b/keys [hash transactions] :as last-block}
                          (-> cryptocurrency ::b/blockchain last)]
                      (b/block (conj transactions transaction) hash))))))))

(s/fdef send-funds
  :args (s/cat :cryptocurrency ::cryptocurrency
               :sender         ::tx/keypair
               :recipient      ::tx/public-key
               :amount         ::tx/value
               :opts           ::tx/options)
  :ret  (s/or ::cryptocurrency ::tx/error))

(defn send-funds
  [cryptocurrency
   ^PGPKeyPair sender
   ^PGPPublicKey recipient
   amount
   opts]
  (let [inputs      (gather-funds cryptocurrency (.getPublicKey sender) amount)
        transaction (tx/transaction sender recipient amount inputs)]
    (transact cryptocurrency transaction opts)))
