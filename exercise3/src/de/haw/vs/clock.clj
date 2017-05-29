(ns de.haw.vs.clock)

(def offset (atom 0))

(def current-time (+ (System/currentTimeMillis) @offset))

(defn current-frame [frame-size]
  (quot current-time frame-size))

(defn current-slot [frame-size slot-count]
  (+ 1 (quot (- (mod current-time frame-size) 1) (/ frame-size slot-count))))

(defn remaining-slots [frame-size slots]
  (- slots (current-slot frame-size slots)))

(defn wait-until-slot-end [slot-size]
  (Thread/sleep (- slot-size (mod current-time slot-size))))
