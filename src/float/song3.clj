(ns float.song3
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
            [float.inst :refer [organ bass sing wobble-organ supersaw my-piano
                                 dub2 reese string bass2 organ2 plucky brass]]
            [overtone.core :refer :all :exclude [tap]]
            [float.play-inst :as player]))

(defmacro play-gated [duration & body]
  `(let [duration-ms# (* 1000 ~duration)
         inst-result# ~@body]
     (at (+ (now) duration-ms#)
         (ctl inst-result# :gate 0))))

(defmacro defperc [name path]
  `(let [buf# (sample ~path)]
     (definst ~name [~(symbol "amp") 1]
       (~'* ~(symbol "amp") (play-buf 2 buf# :action FREE)))))

(def vocal-buffer (sample "resources/samples/loop18_keyFmin_130bpm.wav"))
(definst vocal [ratio 1 amp 1]
  (pitch-shift
   (play-buf 2 vocal-buffer :action FREE)
   :pitch-ratio ratio
   :time-dispersion 0.01))
(vocal )

(def chosen-scale (comp scale/F scale/minor))

(defn as-inst
  "applies the given instrument key to the notes and converts note
  indices to midi"
  [inst phrase]
  (->> phrase
       (wherever :pitch :pitch chosen-scale)
       (all :part inst)))

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
          :clap2 {:sound (fn [& [_]]
                           (do
                                (snare :amp 0.2)
                                (snare2 :amp 0.6)
                                (clap :amp 0.1)))}
          :kick {:sound kick}
          :kick2 {:sound kick2 :amp 0.4}
          :hat {:sound hat :amp 0.15}
          :hat2 {:sound hat2 :amp 0.15}})


(defmethod lz/play-note :plucky [{:keys [pitch duration amp] :as note :or {amp 1}}]
  (when pitch
    (plucky :freq (temperament/equal pitch) :cutoff 900 :dur (* 0.8 duration) :amp amp)))

(defmethod lz/play-note :beat [{:keys [amp drum] :or {amp 1}}] ;; TODO destructure
  (let [{sound-fn :sound default-amp :amp :or {default-amp 0.3}} (get kit drum)]
    (when sound-fn
      (sound-fn :amp (* default-amp amp)))))

(defmethod lz/play-note :bass [{:keys [pitch duration amp] :or {amp 1}}]
  (play-gated duration
   (synth/vintage-bass :note pitch :velocity 30 :amp (* 0.5 amp))))

(defmethod lz/play-note :organ [{:keys [pitch duration]}]
  (organ :freq (temperament/equal pitch) :dur duration))

(defmethod lz/play-note :organ2 [{:keys [pitch duration]}]
  (organ :freq (temperament/equal pitch) :dur duration))

(defmethod lz/play-note :sing [{:keys [pitch duration]}]
  (sing :freq (temperament/equal pitch) :dur duration))

(defmethod lz/play-note :brass [{:keys [pitch duration]}]
  (brass :note pitch :dur duration))

(defmethod lz/play-note :string [{:keys [pitch duration]}]
  (string :note pitch :dur duration))

(defmethod lz/play-note :piano [{:keys [pitch duration]}]
  (when pitch
    (piano/piano pitch :vel 70)))

(defmethod lz/play-note :piano2 [{:keys [pitch duration]}]
  (when pitch
    (play-gated duration
     (my-piano pitch :vel 60 :hard 0.2 :muffle 0.2 :velcurve 0.3))))

(defmethod lz/play-note :piano3 [{:keys [pitch duration]}]
  (when pitch
    (piano/piano pitch :vel 60 :velcurve 0.1 :decay 0.2)))

(defmethod lz/play-note :reese [{:keys [pitch duration]}]
  (when pitch
   (reese :freq (temperament/equal pitch)
          :amp 0.12
          :dur duration)))

(defmethod lz/play-note :supersaw [{:keys [pitch duration amp] :or {amp 1} :as note}]
  (when pitch
    (supersaw (temperament/equal pitch) :amp (* amp 0.5) :dur (- duration 0.05) :release 0.1)))

(defmethod lz/play-note :trumpet [{:keys [pitch duration amp] :or {amp 1}}]
  (when pitch
   (play-gated duration
               #_(sampled-trumpet pitch :start-pos 0.25 :attack 0.01 :level (* amp 1.3))
               (sampled-trumpet pitch :attack 0.05 :level (* 0.3 amp) :start-pos 0.08))))

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

(defn fade-in [notes]
  (let [a (apply min (map :time notes))
        b (apply max (map :time notes))
        fade (fn [n]
               (let [t (:time n)
                     x (/ (- t a)
                          (- b a))]
                 (update n :amp #(* x (or % 1)))))]
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

(def track (atom nil))
(do ;; TODO slow chords and bass
  (reset! track
          [])
  #_(time @(lz/play @track)))


(comment
  ;; i	ii	iii	iv	v	vi	vii
  ;; 0	1	2	3	4	5	6
  ;; -7	-6	5 	-4	-3	-2	-1
  (lz/jam track)
  (lz/stop)
  (lz/play @track)
  (lz/stop)

  (player/play-reese chosen-scale)
  (player/play-piano2 chosen-scale)
  (player/play-trumpet chosen-scale)
  (player/play-supersaw chosen-scale)

  (play-gated 0.2 (synth/vintage-bass :note 33 :velocity 30))
  (supersaw :dur 4)


  (piano/piano 16)

  (let [b (piano/piano 60 :dur 10)]
    (at (+ (now) 200)
        (kill b))
    nil)

  (synth/ks1 :coef 0.8 :decay 20)
  (synth/bubbles)
  (kill synth/bubbles)


  (play-gated 0.2 (sampled-trumpet 60 :attack 0.01 :decay 0.29  :level 10 :sustain 0.1 :start-pos 500))
  (play-gated 1 (sampled-piano 60))
  (play-gated 1 (sampled-trumpet 60 :start-pos 0.25 :attack 0.01 :level 1.3))
)
