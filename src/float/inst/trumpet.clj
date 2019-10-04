(ns float.inst.trumpet
  (:require [clojure.repl :refer [doc]]
            [overtone.core :refer :all]))


(defn- registered-trumpet-samples [] (registered-assets ::TrumpetSamplesMTG))



(def TRUMPET-SAMPLES
  {"trumpet-asharp5.wav"    :A#5
   "trumpet-a5.wav"         :A5
   "trumpet-gsharp5.wav"    :G#5
   "trumpet-g5.wav"         :G5
   "trumpet-fsharp5.wav"    :F#5
   "trumpet-f5.wav"         :F5
   "trumpet-e5.wav"         :E5
   "trumpet-dsharp5.wav"    :D#5
   "trumpet-d5.wav"         :D5
   "trumpet-csharp5.wav"    :C#5
   "trumpet-c5.wav"         :C5
   "trumpet-b4.wav"         :B4
   "trumpet-asharp4.wav"    :A#4
   "trumpet-a4.wav"         :A4
   "trumpet-gsharp4.wav"    :G#4
   "trumpet-g4.wav"         :G4
   "trumpet-fsharp4.wav"    :F#4
   "trumpet-f4.wav"         :F4
   "trumpet-e4.wav"         :E4
   "trumpet-dsharp4.wav"    :D#4
   "trumpet-d4.wav"         :D4
   "trumpet-csharp4.wav"    :C#4
   "trumpet-c4.wav"         :C4
   "trumpet-b3.wav"         :B3
   "trumpet-asharp3.wav"    :A#3
   "trumpet-a3.wav"         :A3
   "trumpet-gsharp3.wav"    :G#3
   "trumpet-g3.wav"         :G3
   "trumpet-fsharp3.wav"    :F#3
   "trumpet-e3.wav"         :E3})

(def notes-samples (into {}
                         (for [[fname notename] TRUMPET-SAMPLES]
                           [(note notename)
                            (load-sample (str "resources/trumpet/" fname))])))

(defonce ^:private silent-buffer (buffer 0))

(def trumpet-index-buffer
  (let [buf (buffer 128 1)]
    (buffer-fill! buf (:id silent-buffer))
    (doseq [[idx note-sample] notes-samples]
      (buffer-set! buf idx (:id note-sample)))
    buf))

;; Maybe raise threshold on sox trimming so attack can be adjusted
(definst sampled-trumpet
  [note 60 level 1 rate 1 loop? 0
   attack 0 decay 1 sustain 1 release 0.1 curve -4 gate 1 start-pos 0.0]
  (let [buf (index:kr (:id trumpet-index-buffer) note)
        rate (:rate trumpet-index-buffer)
        env (env-gen (adsr attack decay sustain release level curve)
                     :gate gate
                     :action FREE)]
    (pan2 :in (* env (scaled-play-buf 1 buf :level level :loop loop?
                                      :start-pos (* rate start-pos)
                                      :action FREE))
          :pos 0)))
