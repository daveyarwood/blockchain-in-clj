(ns blockchain.transaction
  (:require [blockchain.util.encoding :refer (base-64)]
            [clj-pgp.generate         :as    pgp-gen]
            [clj-pgp.signature        :as    pgp-sig]
            [clojure.spec.alpha       :as    s]
            [digest                   :refer (sha-256)])
  (:import [org.bouncycastle.openpgp
            PGPKeyPair PGPPublicKey PGPPrivateKey PGPSignature]))

(s/def ::keypair #(instance? PGPKeyPair %))

(defn generate-keypair ^PGPKeyPair
  []
  (-> (pgp-gen/ec-keypair-generator "secp160r2")
      (pgp-gen/generate-keypair :ecdsa)))

(s/def ::hash string?) ; used as a unique identifier

(s/def ::timestamp pos-int?) ; helps to ensure a unique hash

(s/def ::public-key #(instance? PGPPublicKey %))
(s/def ::sender ::public-key)
(s/def ::recipient ::public-key)

(s/def ::value float?)

(s/def ::input nil?) ; TODO
(s/def ::output nil?) ; TODO
(s/def ::inputs (s/and (s/coll-of ::input) sequential?))
(s/def ::outputs (s/and (s/coll-of ::output) sequential?))

(defn calculate-hash
  [{::keys [sender recipient value timestamp] :as tx}]
  (sha-256 (str (-> sender .getEncoded base-64)
                (-> recipient .getEncoded base-64)
                value
                timestamp)))

(defn correct-hash?
  [{::keys [hash] :as tx}]
  (= hash (calculate-hash tx)))

(s/def ::transaction
  (s/and
    (s/keys :req [::hash ::timestamp ::sender ::recipient ::inputs ::outputs])
    correct-hash?))

(defn transaction
  [sender recipient value inputs]
  (let [timestamp (System/currentTimeMillis)
        tx       {::sender sender
                  ::recipient recipient
                  ::timestamp timestamp
                  ::inputs    inputs
                  ;; TODO
                  ::outputs   [nil]}]
    (assoc tx ::hash (calculate-hash tx))))

(defn sign ^PGPSignature
  [transaction ^PGPPrivateKey private-key]
  (-> transaction calculate-hash (pgp-sig/sign private-key)))

(defn verify-signature
  [transaction ^PGPSignature signature ^PGPPublicKey public-key]
  (-> transaction calculate-hash (pgp-sig/verify signature public-key)))
