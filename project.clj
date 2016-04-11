(defproject genlearn "0.1.0-SNAPSHOT"
  :description "Implementation of higher-order abstraction learning model."
  :url "https://github.com/Scarano/SymLearn"
  :license []
  :target-path "bin"
  :compile-path "%s"
  :source-paths ["src"]
  :java-source-paths ["src"]
  :main genlearn.postsearch
  :jvm-opts ["-Xmx6g" "-server"]
;  :test-paths ["genlearn"]
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/tools.nrepl "0.2.10"]
                 [org.flatland/useful "0.11.3"]
                 [prismatic/plumbing "0.5.0"]
                 [org.apache.commons/commons-math3 "3.2"]
;                 [org.clojure/data.priority-map "0.0.7"]
                 [edu.stanford.nlp/stanford-corenlp "3.5.2"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [clj-stacktrace "0.2.4"]]
;  :resource-paths ["/Users/sam/workspace/stanford-classifier-2015-04-20/stanford-classifier.jar"]
  :repl-options {;:init (require '(clojure.tools.namespace.repl :refer (refresh refresh-all)))
                 :skip-default-init false
                 :caught clj-stacktrace.repl/pst+})
