(ns blockchain.transaction
  (:require [blockchain.util.encoding :refer (base-64 sha-256)]
            [clj-pgp.generate         :as    pgp-gen]
            [clj-pgp.signature        :as    pgp-sig]
            [clojure.spec.alpha       :as    s])
  (:import [org.bouncycastle.openpgp
            PGPKeyPair PGPPublicKey PGPPrivateKey PGPSignature]))

(s/def ::keypair #(instance? PGPKeyPair %))

(defn generate-keypair ^PGPKeyPair
  []
  (-> (pgp-gen/ec-keypair-generator "secp160r2")
      (pgp-gen/generate-keypair :ecdsa)))

;; used as a unique identifier as well as for verification
(s/def ::hash string?)
(s/def ::id ::hash)
(s/def ::transaction-id ::id)

(s/def ::timestamp pos-int?) ; helps to ensure a unique hash

(s/def ::public-key #(instance? PGPPublicKey %))
(s/def ::sender ::public-key)
(s/def ::recipient ::public-key)

(s/def ::value (s/and number? pos?))

(defn calculate-output-hash
  [{::keys [recipient value transaction-id]}]
  (sha-256 (str (-> recipient .getEncoded base-64) value transaction-id)))

(defn correct-output-hash?
  [{::keys [hash] :as output}]
  (= hash (calculate-output-hash output)))

(s/def ::output
  (s/and
    (s/keys :req [::hash ::transaction-id ::recipient ::value])
    correct-output-hash?))

(defn output
  [transaction-id recipient value]
  (let [output' {::transaction-id transaction-id
                 ::recipient      recipient
                 ::value          value}]
    (assoc output' ::hash (calculate-output-hash output'))))

(s/def ::inputs (s/and (s/coll-of ::transaction-id) sequential?))

(defn calculate-transaction-hash
  [{::keys [sender recipient value timestamp] :as tx}]
  (sha-256 (str (-> sender .getEncoded base-64)
                (-> recipient .getEncoded base-64)
                value
                timestamp)))

(defn correct-transaction-hash?
  [{::keys [hash] :as tx}]
  (= hash (calculate-transaction-hash tx)))

(s/def ::signature #(instance? PGPSignature %))

(defn signature-verified?
  "Returns truthy if transaction is signed and the signature is verified with
   the public key."
  [{::keys [hash ^PGPPublicKey sender ^PGPSignature signature] :as transaction}]
  (pgp-sig/verify hash signature sender))

(s/def ::transaction
  (s/and
    (s/keys :req [::hash ::timestamp ::sender ::recipient ::inputs ::signature])
    correct-transaction-hash?
    signature-verified?))

(defn sign ^PGPSignature
  [transaction ^PGPPrivateKey private-key]
  (-> transaction calculate-transaction-hash (pgp-sig/sign private-key)))

(defn transaction
  [^PGPKeyPair sender ^PGPPublicKey recipient value inputs]
  (let [timestamp (System/currentTimeMillis)
        tx        {::sender    (.getPublicKey sender)
                   ::recipient recipient
                   ::timestamp timestamp
                   ::value     value
                   ::inputs    inputs}]
    (assoc tx
           ::hash      (calculate-transaction-hash tx)
           ::signature (sign tx (.getPrivateKey sender)))))

