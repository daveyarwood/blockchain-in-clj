(ns blockchain.util.encoding
  (:require [clojure.data.codec.base64 :as base64]
            [digest                    :as digest]))

;; The functions in this namespace take as input a string and output an encoded
;; string.

(defn base-64
  [s]
  (-> s base64/encode String.))

(def sha-256 digest/sha-256)

