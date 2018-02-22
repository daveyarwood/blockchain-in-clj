(ns blockchain.util.encoding
  (:require [clojure.data.codec.base64 :as base64]
            [digest                    :as digest]))

(defn base-64
  [s]
  (-> s base64/encode String.))

(def sha-256 digest/sha-256)

(defn merkle-root
  "Translated from confusing Java code that apparently will be replaced soon
   \"with an actual merkleroot\", so this might not actually be legit."
  [strs]
  (loop [strs strs]
    (if (<= (count strs) 1)
      (first strs)
      (->> strs
           (partition 2 1)
           (map (fn [[a b]] (sha-256 (str a b))))
           recur))))
