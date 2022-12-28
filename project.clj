(defproject kata-0100-basics-in-15-min "0.1.0"
  :description "basics in clojure"
  :url "http://"
  :license {:name "WTFPL"
            :url "http://www.wtfpl.net/txt/copying/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [io.github.erdos/erdos.assert "0.2.3"]]
  :main ^:skip-aot kata-0100-basics-in-15-min.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
