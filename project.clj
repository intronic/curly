(defproject intronic/curly "0.1.4"
  :description "Clojure utility library"
  :url "https://github.com/intronic/curly"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.5.1"]]

  :source-paths ["src"]
  :test-paths ["test"]

  :profiles {:dev {:dependencies [[expectations "2.0.7"]]}}

  :lein-release {:deploy-via :clojars
                 :scm :git}

  :scm {:name "git"
        :url "https://github.com/intronic/curly"
        :tag ""}

  :global-vars {*warn-on-reflection* true})
