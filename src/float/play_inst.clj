(ns float.play-inst
  (:require [float.keys :as player]
            [overtone.inst.sampled-piano :refer [sampled-piano]]
            [float.inst :as inst]
            [overtone.inst.trumpet :refer [sampled-trumpet]]
            [leipzig.scale :as scale]
            [leipzig.temperament :as temperament]
            [overtone.core :refer [ctl]]))

(defn play-piano [chosen-scale]
 (player/play-inst (fn [{:keys [pitch] :as note}]
                     (when pitch
                       (sampled-piano :note (-> pitch chosen-scale scale/lower)
                                      :attack 0.15)))
                   (fn [active]
                     (when (#{:live :loading} @(:status active))
                       (ctl active :gate 0)
                       nil))))

(defn play-piano2 [chosen-scale]
 (player/play-inst (fn [{:keys [pitch] :as note}]
                     (when pitch
                       (inst/my-piano (chosen-scale pitch) :vel 60 :hard 0.2 :muffle 0.2 :velcurve 0.3)))
                   (fn [active]
                     (when (#{:live :loading} @(:status active))
                       (ctl active :gate 0)
                       nil))))



(defn play-reese [chosen-scale]
 (player/play-inst (fn [{:keys [pitch] :as note}]
                     (when pitch
                       (inst/reese
                        :amp 0.12
                        :dur 4
                        :freq (-> pitch (- 14) chosen-scale temperament/equal))))
                   (fn [active]
                     (when (#{:live :loading} @(:status active))
                       (ctl active :gate 0)
                       nil))))

(defn play-trumpet [chosen-scale]
 (player/play-inst (fn [{:keys [pitch] :as note}]
                     (when pitch
                       (sampled-trumpet (chosen-scale pitch) :attack 0.0 :level 0.3)))
                   (fn [active]
                     (when (#{:live :loading} @(:status active))
                       (ctl active :gate 0)
                       nil))))

(defn play-supersaw [chosen-scale]
 (player/play-inst (fn [{:keys [pitch] :as note}]
                     (when pitch
                       (inst/supersaw (-> pitch chosen-scale temperament/equal) :amp 0.5 :dur 4 :release 0.1)))
                   (fn [active]
                     (when (#{:live :loading} @(:status active))
                       (ctl active :gate 0)
                       nil))))
