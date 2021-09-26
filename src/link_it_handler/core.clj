(ns link-it-handler.core
  (:require [clojure.data.xml :as dxml]
            [clojure.string :as str])
  (:import (clojure.data.xml Element))
  (:gen-class))

(def config
  [{:target-fn :ps-target
    :regex #"IDW PS \d{3}"}
   {:target-fn :isa-target
    :regex #"ISA \[DE\] \d{3}"}])

(defn- verweis-type
  [{:keys [target-fn]}]
  target-fn)

(defmulti create-verweis verweis-type)

(defmethod create-verweis :isa-target
  [{:keys [match node]}]
  (assoc node :content [(dxml/->Element :verweis {:refid (first match)} (:content node))]))

(defn is-target?
  [regex content]
  (re-seq regex content))

(defn mark-target
  [node regex target-fn]
  (let [text (->> (filter string? (:content node))
                  (apply str))]
    {:target-fn target-fn
     :node node
     :match (is-target? regex text)}))

(let [node (dxml/element :absatz {} "Das ist ein ISA [DE] 200 Verweis.")]
 (->> (map #(mark-target node (:regex %) (:target-fn %)) config)
      (filter #(-> %
                   :match
                   nil?
                   not))
      first))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))