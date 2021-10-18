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
  [{:keys [match node text]}]
  (let [before (subs text 0 (str/index-of text (first match)))
        after (subs text (+ (str/index-of text (first match)) (count (first match))) (count text))]
    (assoc node :content (filter not-empty [before (dxml/element :verweis {:refid (first match)} (first match)) after]))))

(defmethod create-verweis :no-target-found
  [{:keys [node]}]
  node)

(defn is-target?
  [regex content]
  (re-seq regex content))

(defn mark-target
  [node regex target-fn]
  (let [text (->> (filter string? (:content node))
                  (apply str))]
    {:target-fn target-fn
     :node node
     :text text
     :match (is-target? regex text)}))

(defn get-first-match
  [matches]
  (first 
   (filter #(-> %
               :match
               nil?
               not) matches)))

(let [node (dxml/element :absatz {} "Das ist ein ISA [DE] 20 Verweis.")]
 (->> (map #(mark-target node (:regex %) (:target-fn %)) config)
      (filter #(-> %
                   :match
                   nil?
                   not))
      first))

(let [node (dxml/element :absatz {} "Das ist ein ISA [DE] 200 Verweis.")]
  (-> (map #(mark-target node (:regex %) (:target-fn %)) config)
      vec
      (conj {:target-fn :no-target-found
             :node node
             :match (:content node)})
      (get-first-match)
      (create-verweis)))

(defn handle-node
  [node]
  (cond
    (.isInstance Element node) (map handle-node (:content node))
    (string? node) "Handle verweis"))

(->>
 (dxml/->Element
 :absatz
 {}
 '("Das ist ein "
  (dxml/->Element :span {} "ISA ")
  (dxml/->Element :b {} "[DE]")
  " 200 Verweis."))
 handle-node)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))