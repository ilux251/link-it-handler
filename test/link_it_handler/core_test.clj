(ns link-it-handler.core-test
  (:require [clojure.test :refer :all]
            [link-it-handler.core :refer :all]
            [clojure.data.xml :as dxml]))


(deftest is-isa-verweis-test
  (testing "ISA Verweis vorhanden"
    (is (is-target? #"ISA \[DE\] \d{3}" "Es ist ein ISA [DE] 200 Verweis vorhanden."))))

(deftest no-isa-target-test
  (testing "Kein ISA Verweis vorhanden"
    (is (not (is-target? #"ISA \[DE\] \d{3}" "Es ist kein ISA Verweis vorhanden.")))))

(deftest more-than-on-isa-target-test
  (testing "Es ist mehr als ein ISA Target vorhanden"
    (is (-> (is-target? #"ISA \[DE\] \d{3}" "Es ist ein ISA [DE] 200 Verweis vorhanden und noch ein weiterer ISA [DE] 500.")
            count
            (> 1)))))

(deftest is-ps-verweis-test
  (testing "PS Verweis vorhanden"
    (is (is-target? #"IDW PS \d{3}" "Es ist ein IDW PS 700 Verweis vorhanden."))))

(deftest mark-isa-target-test
  (testing "Es wurde ein ISA Target im Text gefunden und es wird eine Map zurückgegeben."
    (is (= (mark-target (dxml/element :absatz {} "ISA [DE] 200") #"ISA \[DE\] \d{3}" :isa-target)
           {:target-fn :isa-target
            :node #clojure.data.xml.Element{:tag :absatz :attrs {} :content ["ISA [DE] 200"]}
            :match ["ISA [DE] 200"]}))))

(deftest create-isa-target-test
  (testing "ISA Verweis erstellen"
    (is (= (create-verweis (first (filter #(not (nil? (:match %))) [(mark-target (dxml/element :absatz {} "ISA [DE] 200") #"ISA \[DE\] \d{3}" :isa-target)])))
           (dxml/element :absatz {} (dxml/element :verweis {:refid "ISA [DE] 200"} "ISA [DE] 200"))))))