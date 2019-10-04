(ns float.song2
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
            [overtone.inst.sampled-flute :refer [sampled-non-vibrato-flute]]
            [float.inst.trumpet :refer [sampled-trumpet]]
            [float.inst :refer [organ bass sing wobble-organ supersaw
                                 dub2 reese string bass2 organ2 plucky]]
            [overtone.core :refer :all :exclude [tap]]
            [float.keys :as player]))

(defmacro play-gated [duration & body]
  `(let [duration-ms# (* 1000 ~duration)
         inst-result# ~@body]
     (at (+ (now) duration-ms#)
         (ctl inst-result# :gate 0))))

(defmacro defperc [name path]
  `(let [buf# (sample ~path)]
     (definst ~name [~(symbol "amp") 1]
       (~'* ~(symbol "amp") (play-buf 2 buf# :action FREE)))))

(def chosen-scale (comp scale/A scale/minor))

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
          :kick2 {:sound kick2 :amp 0.4}
          :hat {:sound hat :amp 0.15}
          :bass-808 {:sound bass-808}
          :hat2 {:sound hat2 :amp 0.15}})

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
  (when pitch
    (piano/piano pitch :vel 70)))

(defmethod lz/play-note :piano2 [{:keys [pitch duration]}]
  (when pitch
    (piano/piano pitch :vel 60 :hard 0.2 :muffle 0.2 :velcurve 0.3)))

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
              :amp (or amp 0.5)
              :dur duration)))

(defmethod lz/play-note :trumpet [{:keys [pitch duration amp] :or {amp 1}}]
  (when pitch
   (play-gated duration
               #_(sampled-trumpet pitch :start-pos 0.25 :attack 0.01 :level (* amp 1.3))
               (sampled-trumpet pitch :attack 0.0 :level (* amp 1.3)))))

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
             :iii7 (chord/root chord/seventh (+ chord-shift 2))
             :iii-inv (-> chord/triad (chord/root (+ chord-shift 2)) (chord/inversion 1))
             :v7 (-> chord/seventh (chord/root (+ chord-shift 4)) (chord/inversion 3))
             :v (-> chord/triad (chord/root (+ chord-shift 4)))
             :VM (-> chord/triad (chord/root (+ chord-shift 4))
                     (assoc :iii -0.5)) ;; Major chord
             :iv (chord/root chord/triad (+ chord-shift 3))
             :iv7 (chord/root chord/seventh (+ chord-shift 3))
             :vi (-> chord/triad (chord/root (+ chord-shift 5)))
             :vi7 (-> chord/seventh (chord/root (+ chord-shift 5)))})

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
                        [(tap :kick2 [0 1.5 3 ] 4)
                         (tap :snare [1] 4)
                         (tap :hat (range 0 4 0.5) 4)])
                (all :part :beat)
                (times 2)))

(def build-drum (->>
                 (reduce with
                         [(tap :kick2 [0 1.5 4 5] 8)
                          (tap :hat (range 8) 8)
                          (tap :hat2 [0.505 2.495 4.505 5.5] 8)])
                 (all :part :beat)

                 (times 2)))

#_(->> base-drum (times 3) (tempo (bpm 120)) lz/play)

(def track (atom nil))
(do
  (reset! track
          (let [chords1 (inst-phrase :piano
                                     (take 8 (interleave (repeat 3.5) (repeat 0.5)))
                                     (map chords [:v nil :iv7 nil :vi7 nil :i nil]))
                chords2 (inst-phrase :piano [1 1] [{:i -3 :iii -0.5 :v 1} (-> chord/triad (chord/root 0))])]


            (->>
             (times 2 chords1)
             (with melody1)
             (with (times 4 base-drum))
             (tempo (bpm 120)))))
  (time @(lz/play @track)))

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

  (player/play-inst (fn [{:keys [pitch] :as note}]
                      (when pitch
                        (sampled-trumpet (-> pitch chosen-scale))))
                    (fn [active]
                      (when (#{:live :loading} @(:status active))
                        (ctl active :gate 0)
                        nil)))

  (do
    (recording-start "/Users/jonathan/src/music/float/song2.wav")
    @(lz/play @track)
    (recording-stop))

  (piano/piano 16)

  (let [b (piano/piano 60 :dur 10)]
    (at (+ (now) 200)
        (ctl (:id b) :gate 0))
    nil)

  (let [bass (synth/daf-bass :freq 220 :amp 0.2)
        bass2 (synth/daf-bass :freq 330 :amp 0.2)]
      (at (+ (now) 200)
          (ctl bass :gate 0))
      (at (+ (now) 800)
          (ctl bass2 :gate 0))
      nil)
  (kill synth/daf-bass)


  (synth/ks1 :coef 0.8 :decay 20)
  (synth/bubbles)
  (kill synth/bubbles)

  (play-gated 0.2
   (sampled-piano ))

  (let [playing (sampled-piano 72)]
      (at (+ (now) 200)
          (ctl playing :gate 0))
      nil)

  (play-gated 0.2 (sampled-trumpet 60 :attack 0.01 :decay 0.29  :level 10 :sustain 0.1 :start-pos 500))
  (play-gated 1 (sampled-trumpet 60))
  (play-gated 1 (sampled-trumpet 60 :start-pos 0.25 :attack 0.01 :level 1.3))
)
