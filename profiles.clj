{:user
 {:plugins [[refactor-nrepl "2.5.0"]
            [cider/cider-nrepl "0.25.0-alpha1"]
            [lein-auto "0.1.3"]]
  :dependencies [[profit "0.1.0-SNAPSHOT"]]
  :injections   [(require '$)
                 ;; (#'clojure.core/load-data-readers)
                 ;; (set! *data-readers* (.getRawRoot #'*data-readers*))
                 ($/set-generic-tags!)]}}
