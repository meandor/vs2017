(ns de.haw.vs.data-access.specs
  (:require [clojure.spec.alpha :as s]))

(s/def ::datagram #(and (bytes? %) (= 34 (alength %))))

(s/def ::station-class #(or (= % "A") (= % "B")))
(s/def ::payload string?)
(s/def ::slot pos-int?)
(s/def ::send-time pos?)

(s/def ::message (s/keys :req [::station-class ::payload ::slot ::send-time]))