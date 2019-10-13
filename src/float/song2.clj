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
            [overtone.inst.trumpet :refer [sampled-trumpet]]
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

(def chosen-scale (comp scale/A scale/minor))

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

(defmethod lz/play-note :piano2 [{:keys [pitch duration amp] :or {amp 1}}]
  (when pitch
    (play-gated duration
     (my-piano pitch :vel 60 :hard 0.2 :muffle 0.2 :velcurve 0.3 :amp amp))))

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
               (sampled-trumpet pitch :attack 0.05 :level (* 0.3 amp) :start-pos 0.1))))

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
                     (assoc :iii -0.5)) ;; Major dominant
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

(def bass-note #(when % (- % 14)))
(def chords1 (inst-phrase :piano2
                          (take 16 (interleave (repeat 3.9) (repeat 0.1)))
                          (map chords [:VM nil :VM nil
                                       :iv7 nil :iv7 nil
                                       :vi7 nil :vi7 nil
                                       :i nil :i nil])))

(def bass1 (inst-phrase :reese
                        [2  1 1 4 1 1 2 4]
                        (concat (map bass-note [-3 -0.5 -3]) [nil] (map bass-note [-2 0 -2 0]))))

(def melody1 (inst-phrase :trumpet
                          [4 eth sth eth qtr qtr (* 3 sth)
                          8
                          qtr qtr eth eth eth eth 4
                          qtr qtr eth eth eth eth eth eth eth 5/2]
                         [nil -3  nil  -3  1   -3 nil
                          nil
                          -2 0 -2 -2 -3 -3 nil
                          -3 0 2 2 0 0 0 0 2 0 nil]))

(def intro (with
            (inst-phrase :piano2
                         [qtr 3
                          eth eth 3
                          eth eth 3
                          eth eth 3]
                         [4 6.5 5 4 3 5 4 3 4 2 0])
            (inst-phrase :piano2
                         (take 8 (interleave (repeat 3.9) (repeat 0.1)))
                         (map chords [:VM nil :iv7 nil :vi7 nil :i nil]))))

(def melody1-saw (let [rhythm (concat (repeat 8 eth) [3.5 0.5]
                                      (repeat 16 eth)
                                      (repeat 16 eth)
                                      (repeat 16 eth))]
                   (with
                    (inst-phrase :supersaw
                                 rhythm
                                 [-3 nil -3 nil -3 nil -3 nil -3 nil
                                  -4 nil -4 nil -4 nil -4 nil -4 nil -4 nil -4 nil -4 nil
                                  -2 nil -2 nil -2 nil -2 nil -2 nil -2 nil -2 nil -2 nil
                                  0 nil 0 nil 0 nil 0 nil   0 0 0 0 -1 -1 0 0])
                    (inst-phrase :supersaw
                                 rhythm
                                 [1 nil 1 nil 1 nil 1 nil 1 nil
                                  0 nil 0 nil 0 nil 0 nil 0 nil 0 nil 0 nil 0 nil
                                  0 nil 0 nil 0 nil 0 nil 0 nil 0 nil 0 nil 0 nil
                                  2 nil 2 nil 2 nil 2 nil nil nil 2 2 2 2 2 2]))))

(def slow-bass (->> bass1
                    (where :duration (partial * 2))
                    (where :time (partial * 2))))

(def drop-phrase (phrase
                  [hf qtr qtr
                   hf qtr qtr]
                  (concat (map chords [:iv :vi :i
                                       nil :v]) [{:i -6 :v -2}])))

(def plucky-drop (->> drop-phrase
                      (as-inst :plucky)
                      (where :pitch scale/low)))

(def saw-drop (->> drop-phrase
                   (as-inst :supersaw)))

(def drops-bass (with
                 (inst-phrase :bass
                              [hf qtr
                               (+ qtr hf) qtr qtr]
                              (map bass-note [-2 0
                                              2 -1 1]))
                 (inst-phrase :bass
                              [hf qtr
                               (+ qtr hf) qtr qtr]
                              (map #(+ -21 %) [-4 -2
                                               0 -3 -2.5]))))

(def light-drums (->> [(tap :kick2 [0 1 4] 8)
                       (tap :snare [ 6] 8)
                       (tap :hat [1 2 3 3.5 5 6 7 7.5] 8)
                       (tap :hat2 (concat (range 1 4 0.5) (range 4.5 8 0.5)) 8)]
                      (reduce with)
                      (all :part :beat)
                      (all :amp 0.6)))



(def chords1-piano (->>
                    (inst-phrase :piano2
                                 (take 16 (interleave (repeat 3.9) (repeat 0.1)))
                                 (map chords [:VM nil :VM nil
                                              nil nil :iv7 nil
                                              :vi7 nil :vi7 nil
                                              :i nil :i nil]))))

(def melody2-piano
 (inst-phrase :piano2
              [4 qtr qtr qtr qtr
               4 4
               qtr qtr hf qtr qtr hf
               qtr qtr qtr qtr qtr qtr qtr qtr]
              [nil
               nil -0.5 1 2
               3 nil
               nil 5 7 nil 4 5
               nil 4 2 0 nil 4 2 0]))

(def outro (->> (where :pitch scale/low
                   (inst-phrase :piano2
                                [eth 4.5         qtr          hf]
                                [7   (chords :i) (chords :VM) (chords :i)]))
                  (with (->>
                         (inst-phrase :piano2
                                      [4   sth sth sth sth sth sth sth sth    sth sth eth qtr]
                                      [nil 0   2   0   2   4   5   4   3      4   5   4   7])
                         (where :duration #(* % 1.6))
                         (where :pitch scale/high)))))

(def track (atom nil))
(do
  (reset! track
          (let [full-drop (->> base-drum
                               (with (->> (tap :clap [6 7] 8)
                                          (all :part :beat))
                                     drops-bass
                                     plucky-drop)
                               (times 2)
                               (then (->> base-drum
                                          (with (->> (tap :clap [5 7] 8) ;; TODO vary these drums
                                                     (all :part :beat))
                                                drops-bass)
                                          (times 4)
                                          (with (fade-out (times 4 plucky-drop)))
                                          (with (fade-in (times 4 saw-drop)))))
                               (then (->> base-drum
                                          (with (->> (tap :clap [5 7] 8)
                                                     (all :part :beat))
                                                (inst-phrase :piano2
                                                             [eth eth 5 qtr qtr]
                                                             [(chords :iv) (chords :iv) nil {:i -3 :vii 3} {:i -3 :v 1}])
                                                drops-bass
                                                saw-drop)
                                          (times 2))))]
            (->> (with intro base-drum)
                 (then
                  (->> base-drum
                       (times 4)
                       (with chords1 slow-bass melody1) ;; TODO sharper trumpet
                       (times 2)))
                 (then
                  (->> base-drum
                       (times 4)
                       (with (filter #(< (:time %) 24) slow-bass))
                       (with melody1-saw)))

                 ;; drop/chorus
                 (then full-drop)

                 ;; second build
                 (then (with chords1-piano melody2-piano (times 4 light-drums)))
                 (then (with chords1-piano melody2-piano slow-bass (times 4 light-drums)))
                 (then (with chords1-piano melody1-saw slow-bass (times 4 light-drums)))
                 (then (with melody1-saw
                             (filter #(< (:time %) 24) slow-bass)
                             (times 4 base-drum)))

                 ;; second drop
                 (then full-drop)
                 (then (-> outro
                           (concat  [{:time 9 :duration 1 :part :piano2}]) ;; rest so the last notes will be audible
                           fade-out))

                 (tempo (bpm 120)))))
  #_(time @(lz/play @track)))

(reset! track light-drums)

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


  (do
    (recording-start "/Users/jonathan/src/music/float/VisionFromAHighPlace.wav")
    @(lz/play @track)
    (recording-stop))

  (piano/piano 16)

  (let [b (piano/piano 60 :dur 10)]
    (at (+ (now) 200)
        (kill b))
    nil)

  (synth/ks1 :coef 0.8 :decay 20)
  (synth/bubbles)
  (kill synth/bubbles)


  (play-gated 0.5 (sampled-trumpet 60 :attack 0.01 :decay 0.29  :level 20 :sustain 0.1))
  (play-gated 1 (sampled-piano 60))
  (play-gated 1 (sampled-trumpet 60 :start-pos 0.25 :attack 0.01 :level 1.3))
)
