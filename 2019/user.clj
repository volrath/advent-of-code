(ns user
  (:require [clj-http.client :as http]
            [clj-http.cookies :as cookies]
            [clojure.edn :as edn]))


(defn start-day [day-nr]
  (let [cookie-store (cookies/cookie-store)
        cookie (cookies/to-basic-client-cookie (edn/read-string (slurp ".cookies.edn")))]
    (.setDiscard cookie false)
    (cookies/add-cookie cookie-store cookie)
    (spit (str "input/day" day-nr)
          (-> "https://adventofcode.com/2019/day/%s/input"
              (format day-nr)
              (http/get {:cookie-store cookie-store})
              :body))))
