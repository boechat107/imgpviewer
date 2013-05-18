(defproject org.clojars.boechat107/image-processing "2.0.0-SNAPSHOT"
  :description "An image processing library using Clojure data structures."
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [incanter/incanter-core "1.5.0-SNAPSHOT"]
                 [net.mikera/imagez "0.0.2"]
                 [seesaw "1.4.3"]]
  :resource-paths ["test/"]
  :global-vars {*warn-on-reflection* true, *assert* false}
  :jar-exclusions [#"(?:^|/).svn/"])
