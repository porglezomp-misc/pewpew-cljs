(defproject pewpew "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :min-lein-version "2.6.1"

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.198"]
                 [org.clojure/core.async "0.2.385"
                  :exclusions [org.clojure/tools.reader]]
                 #_[com.taoensso/sente "1.10.0"]
                 [funcool/tubax "0.2.0"]
                 [cljs-http "0.1.41"]
                 #_[devcards "0.2.1-7"]
                 [impi "0.0.2"]
                 #_[weavejester/pixi "4.0.0-8fdb17a-0"]]

  :plugins [[lein-figwheel "0.5.4-7"]
            [lein-cljsbuild "1.1.3" :exclusions [[org.clojure/clojure]]]]

  :source-paths ["src"]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]

  :cljsbuild {:builds
              [{:id "dev"
                :source-paths ["src"]
                :figwheel {:on-jsload "pewpew.core/on-js-reload"}
                :compiler {:main pewpew.core
                           :asset-path "js/compiled/out"
                           :output-to "resources/public/js/compiled/pewpew.js"
                           :output-dir "resources/public/js/compiled/out"
                           :source-map-timestamp true
                           :preloads [devtools.preload]}}
               {:id "devcards"
                :source-paths ["src"]
                :figwheel {:on-jsload "pewpew.core/on-js-reload"
                           :devcards true}
                :compiler {:main pewpew.core
                           :asset-path "js/compiled/devcards_out"
                           :output-to "resources/public/js/compiled/pewpew_devcards.js"
                           :output-dir "resources/public/js/compiled/devcards_out"
                           :source-map-timestamp true
                           :preloads [devtools.preload]}}
               {:id "min"
                :source-paths ["src"]
                :compiler {:output-to "resources/public/js/compiled/pewpew.js"
                           :main pewpew.core
                           :optimizations :advanced
                           :pretty-print false}}]}

  :figwheel {:css-dirs ["resources/public/css"]
             :open-file-command "emacsclient"}


  ;; setting up nREPL for Figwheel and ClojureScript dev
  ;; Please see:
  ;; https://github.com/bhauman/lein-figwheel/wiki/Using-the-Figwheel-REPL-within-NRepl


  :profiles {:dev {:dependencies [[binaryage/devtools "0.8.1"]
                                  [figwheel-sidecar "0.5.4-7"]
                                  [com.cemerick/piggieback "0.2.1"]]
                   ;; need to add dev source path here to get user.clj loaded
                   :source-paths ["src" "dev"]
                   ;; for CIDER
                   :plugins [[cider/cider-nrepl "0.14.0-SNAPSHOT"]]
                   :repl-options {; for nREPL dev you really need to limit output
                                  :init (set! *print-length* 50)
                                  :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}}

)
