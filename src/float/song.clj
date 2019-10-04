(ns float.song
  (:require [clojure.java.io :as io]
            [clojure.repl :refer [doc]]
            [float.boot :refer [connected]]
            [leipzig.melody :refer :all]
            [leipzig.live :as lz]
            [leipzig.scale :as scale]
            [leipzig.melody :as melody]
            [leipzig.chord :as chord]
            [leipzig.temperament :as temperament]
            [leipzig.scale :as scale]
            [overtone.inst.piano :as piano]
            [overtone.inst.synth :as synth]
            [overtone.inst.drum :as drum]
            [overtone.inst.sampled-piano :refer [sampled-piano]]
            [float.inst :refer [organ bass sing wobble-organ supersaw
                                 dub2 reese string bass2 organ2 plucky]]
            [overtone.core :refer :all :exclude [tap]]
            [float.keys :as player]))


(defmacro defperc [name path]
  `(let [buf# (sample ~path)]
     (definst ~name [~(symbol "amp") 1]
       (~'* ~(symbol "amp") (play-buf 2 buf# :action FREE)))))

(def chosen-scale (comp scale/C scale/major))

(defn as-bass [phrase]
  (->> phrase
       (where :pitch chosen-scale)
       (all :part :bass)
       (all :amp 1)))

(defn as-organ [phrase]
  (->> phrase
       (where :pitch chosen-scale) ;; index->midi
       (all :part :organ)
       (all :amp 1)))

(defn as-piano [phrase]
  (->> phrase
       (where :pitch chosen-scale) ;; index->midi
       (all :part :piano)))

(defn as-inst
  "applies the given instrument key to the notes and converts note
  indices to midi"
  [inst phrase]
  (->> phrase
       (wherever :pitch :pitch chosen-scale)
       (all :part inst)))

#_(def rizer-sample (load-sample "resources/samples/BUILDS/SYNTH RIZERS/Rizer 3 trim.wav"))
#_(definst rizer-inst [amp 1 rate 1]
    (* amp (scaled-play-buf :num-channels 2 :buf-num (:id rizer-sample) :rate rate)))

;; (def buf-808 (sample "resources/samples/BASS/Dirty 808.wav"))
(def buf-808 (sample "resources/samples/BASS/Punchy 808.wav"))
(definst bass-808 [amp 1 pitch 32.7 duration 4.5]
  (let [amp (* 2 amp)
        sample-pitch 32.7
        ratio (/ pitch sample-pitch)
        amp-env (env-gen (adsr-ng :attack 0
                                  :attack-level amp
                                  :decay 0
                                  :sustain duration
                                  :release 0
                                  :level amp) :action FREE)]
    #_(* amp-env (play-buf 2 buf-808 :action FREE))
    (* amp-env (pitch-shift :in (play-buf 2 buf-808 :action FREE)
                            :pitch-ratio ratio
                            :time-dispersion 0.001))))
(comment
 (bass-808 )
 (bass-808 :pitch 32.7 :amp 5)
 (bass-808 :pitch 55))


(defperc snare "resources/samples/SNARES&CLAPS/Auveregne Snare.wav")
(defperc snare2 "resources/samples/SNARES&CLAPS/Death Speaks Snare.wav")
(defperc clap "resources/samples/SNARES&CLAPS/Soul Clap.wav")
(defperc kick "resources/samples/KICKS/Top Ramen Kick.wav")
(defperc kick2 "resources/samples/KICKS/MOTM Kick.wav")
(defperc hat "resources/samples/HATS/Lost Dreams Hat.wav")
(defperc hat2 "resources/samples/HATS/Swoosh Hi Hat.wav")
(def kit {:snare {:sound snare}
          :snare2 {:sound snare2}
          :clap {:sound clap}
          :kick {:sound kick}
          :kick2 {:sound kick2}
          :hat {:sound hat}
          :bass-808 {:sound bass-808}
          :hat2 {:sound hat2}})

;; (rizer-inst :rate (float (/ 128 100)))

;; (lz/play-note {:drum :close-hat :part :beat :amp 1})
; (rizer-inst)


(defmethod lz/play-note :plucky [{:keys [pitch duration amp] :as note :or {amp 1}}]
  (when pitch
    (plucky :freq (temperament/equal pitch) :cutoff 900 :dur duration :amp amp)))

(defmethod lz/play-note :bass-808 [{:keys [pitch duration amp] :as note}]
  (when pitch
    (bass-808 :pitch pitch
              :duration (or duration 4.5)
              :amp (or amp 1))))

(defmethod lz/play-note :beat [note] ;; TODO destructure
  (when-let [fn (-> (get kit (:drum note)) :sound)]
    (let [default-amp (-> (get-in kit [(:drum note) :amp] 0.3))]
     (fn :amp (* default-amp (:amp note 1))))))

(defmethod lz/play-note :bass [{:keys [pitch duration]}]
  (synth/bass :freq (/ pitch 2) :t (* 2 duration)))

(defmethod lz/play-note :organ [{:keys [pitch duration]}]
  (organ :freq (temperament/equal pitch) :dur duration))

(defmethod lz/play-note :piano [{:keys [pitch duration]}]
  (piano/piano pitch :vel 70))

(defmethod lz/play-note :piano2 [{:keys [pitch duration]}]
  (when pitch
    (piano/piano pitch :vel 60 :hard 0.2 :muffle 0.2 :velcurve 0.3)))

(defmethod lz/play-note :piano [{:keys [pitch duration]}]
  (when pitch
    (piano/piano pitch :vel 70)))

(defmethod lz/play-note :piano3 [{:keys [pitch duration]}]
  (when pitch
    (piano/piano pitch :vel 60 :velcurve 0.1 :decay 0.2)))

(defmethod lz/play-note :reese [{:keys [pitch duration]}]
  (when pitch
   (reese :freq (temperament/equal pitch)
          :amp 0.12
          :dur duration)))

(defmethod lz/play-note :supersaw [{:keys [pitch duration amp] :as note}]
  (when pitch
    (supersaw :freq (temperament/equal pitch)
              :amp (or amp 0.4)
              :dur duration)))

(defmethod lz/play-note :rest [_] nil)

(defmethod lz/play-note :dub-inst [{:keys [pitch]}]
  (dub2 :freq (temperament/equal  (- pitch 36))))

(defn tap [drum times length ]
  (map #(zipmap [:time :duration :drum]
                [%1 (- length %1) drum]) times))

(defn fade-out [notes]
  (let [a (apply min (map :time notes))
        b (apply max (map :time notes))
        fade (fn [n]
               (let [t (:time n)
                     x (/ (- t a)
                          (- b a))]
                 (update n :amp #(* (- 1 x) (or % 1)))))]
    (map fade notes)))

(def chord-shift -7)
(def chords {:i (chord/root chord/triad (+ chord-shift 0))
             :ii (chord/root chord/triad (+ chord-shift 1))
             :iii (chord/root chord/triad (+ chord-shift 2))
             :iii-inv (-> chord/triad (chord/root (+ chord-shift 2)) (chord/inversion 1))
             :v7 (-> chord/seventh (chord/root (+ chord-shift 4)) (chord/inversion 3))
             :v (-> chord/triad (chord/root (+ chord-shift 4)))
             :iv (chord/root chord/triad (+ chord-shift 3))
             :vi (-> chord/triad (chord/root (+ chord-shift 5)))})

(defn inst-phrase [inst times notes]
  (as-inst inst (phrase times notes)))

(def hf  2)
(def qtr 1)
(def eth 1/2)
(def sth 1/4)
(def swup (partial * 1.05))
(def swbk (partial * 0.95))

(def base-drum (->>
                (reduce with
                        [(tap :kick2 (range 0 16 2) 16)
                         (tap :kick [2.5 10.5] 16)
                         (tap :hat (range 0 16 qtr) 16)
                         (tap :hat2 [3.495 5.505 11.495 13.505] 16)])
                (all :part :beat)))

(def build-drum (->>
                 (reduce with
                         [(tap :kick [2.5 10.5 14] 16)
                          (tap :hat (range 0 16 qtr) 16)
                          (tap :hat (range 14 16 eth) 16)
                          (tap :hat2 [3.495 5.505 11.495 13.505 14.507 15.253 15.512] 16)])
                 (all :part :beat)))

(def track (atom nil))
(reset! track
        (let [melody1 (inst-phrase :plucky
                                   [qtr eth eth
                                    eth eth eth eth
                                    eth eth eth eth
                                    qtr eth eth

                                    eth eth eth eth
                                    eth eth qtr
                                    (swup sth) (swbk sth) eth eth eth
                                    eth eth eth eth]
                                   [2 4 6
                                    2 2 2 4
                                    2 2 2 4
                                    2 4 6
                                    1 1 1 1
                                    1 1 3
                                    2 1 1 1
                                    1 1 3 3])
              chords1 (inst-phrase :plucky
                                   [4 4 4 1 1 1 1]
                                   (map chords [:iii :iii :ii :ii nil :ii nil]))
              melody2 (inst-phrase :plucky
                                   [qtr eth eth
                                    qtr eth eth
                                    eth eth eth eth
                                    qtr eth eth
                                    qtr qtr
                                    qtr qtr
                                    qtr qtr
                                    (+ eth (swup eth)) (swbk eth) eth

                                    qtr qtr
                                    qtr qtr
                                    qtr qtr
                                    (+ eth (swup eth)) (swbk eth) eth

                                    qtr qtr
                                    qtr qtr
                                    qtr qtr
                                    (+ eth (swup eth)) (swbk eth) eth]
                                   [4 3 4
                                    5 6 6
                                    5 4 4 4
                                    3 4 4
                                    4 3 4 4
                                    3 4 4 4 4

                                    3 1 1 1
                                    1 1 1 2 3
                                    4 3 4 4
                                    3 4 4 4 4])
              phrase-v (inst-phrase :plucky
                                    [4 1/2 1/2 1/2 1/2 1/2 3/2]
                                    (map chords [:v7 :v nil :v7 nil :v7 nil]))
              phrase-ii (inst-phrase :plucky
                                     [4 1/2 1/2 1/2 1/2 1/2 3/2]
                                     (map chords [:ii :ii nil :ii nil :ii nil]))
              chords2 (->> phrase-v (times 2) (then phrase-ii) (then phrase-v))

              melody3 (inst-phrase :plucky
                                   (interleave (repeat 16 (swup eth))
                                               (repeat 16 (swbk eth)))
                                   [4 4 3 2 2 2 2 2
                                    4 4 3 2 2 2 nil nil
                                    3 3 3 3 2 1 1 1
                                    5 5 5 5 3 2 1 1])
              bridge-drum (->> [(tap :hat (range 0 16 qtr) 16)
                                (tap :hat2 (range (swup 0.5) 16 qtr) 16)]
                               (reduce with)
                               (all :part :beat))
              build2-drum (->> [(tap :kick2 (range 16) 16)
                                (tap :kick2 (range (+ 8 (swup eth)) 16) 16)
                                (tap :hat2 (range (+ 8 (swup eth)) 16) 16)
                                (tap :hat (range (+ 8 (swup eth) sth) 16 2) 16)]
                               (reduce with)
                               (all :part :beat))
              twelve-bar-rest (inst-phrase :beat [48] [nil])]
          (->> (with melody1 chords1)
               (then (times 2 (with melody1 chords1 base-drum)))
               (then (with melody1 chords1 build-drum))

               (then (times 2 (with melody2 chords2 (times 2 base-drum))))

               (then (->> (times 4 melody3)
                          (with (fade-out (times 4 bridge-drum)))
                          (with (->> twelve-bar-rest (then build2-drum)))))

               (then (with (->> (with melody2 chords2)
                                (then (->> (with melody2 chords2) fade-out )))
                           (->> (times 2 base-drum)
                                (then (fade-out (times 2 bridge-drum))))))

               (tempo (bpm 98)))))

(comment
  ;; i  ii iii iv v  vi vii
  ;; 0  1  2   3  4  5  6
  ;; -7 -6 -5  -4 -3 -2 -1
  (lz/jam track)
  (lz/stop)
  (lz/play @track)
  (lz/stop)

  (player/play-inst (fn [note]
                      (when (:pitch note)
                       (-> note
                           (assoc :part :plucky)
                           (update :pitch chosen-scale)
                           (assoc :duration 0.25)
                           lz/play-note))))

  (time @(lz/play @track))

  (->>
   (phrase
    [hf hf hf hf hf hf 4]
    (map chords [:iii :iii :vi :vi :ii :ii :ii]))
   (as-inst :supersaw)
   (tempo (bpm 110))
   (where :pitch (comp scale/high scale/high))
   lz/play)



  (- 9.555 0.166)

  (bass2 :amp 0.3)
  (reese    (temperament/equal (chosen-scale -13)))


  (do
    (recording-start "/Users/jonathan/src/music/float/float.wav")
    @(lz/play @track)
    (recording-stop))

  (piano/piano 16))
