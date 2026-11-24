(defproject blood-donor-registry "0.1.0-SNAPSHOT"
  :description "Information system for recording and rewarding voluntary blood donors at blood drives organized by the Red Cross of Belgrade municipal branches"
  :url "https://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.12.2"]]
  :main ^:skip-aot blood-donor-registry.core
  :target-path "target/%s"
  :profiles {:dev {:dependencies [[midje "1.10.10"]]
                   :plugins [[lein-midje "3.2.2"]]}
             :uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
