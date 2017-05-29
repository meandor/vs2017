(ns de.haw.vs.clock)

(def offset (atom 0))

(defn current-time []
  (+ (System/currentTimeMillis) @offset))

(defn current-frame [frame-size]
  (quot (current-time) frame-size))

(defn current-slot [frame-size slot-count]
  (+ 1 (quot (- (mod (current-time) frame-size) 1) (/ frame-size slot-count))))

(defn remaining-slots [frame-size slot-count]
  (- slot-count (current-slot frame-size slot-count)))

(defn wait-until-slot-end [slot-duration]
  (Thread/sleep (- slot-duration (mod (current-time) slot-duration))))

(defn ms-until-slot-middle [frame-size slot-count slot-number]
  (let [duration-per-slot (/ frame-size slot-count)]
    (->> (current-slot frame-size slot-count)
         (- slot-number)                                    ; slot offset
         (* duration-per-slot)                              ; time offset for the desired slot
         (+ (- (/ duration-per-slot 2) (mod (current-time) duration-per-slot)))) ; time offset for middle of desired slot
    ))
