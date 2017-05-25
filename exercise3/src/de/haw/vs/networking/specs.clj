(ns de.haw.vs.networking.specs
  (:require [clojure.spec.alpha :as s]))

(s/def ::datagram #(and (bytes? %) (= 34 (alength %))))

(s/def ::station-class #(or (= % "A") (= % "B")))
(s/def ::station-name string?)
(s/def ::payload string?)
(s/def ::payload-content string?)
(s/def ::slot pos-int?)
(s/def ::send-time pos?)

(s/def ::message (s/keys :req [::station-class ::station-name ::payload-content ::payload ::slot ::send-time]))