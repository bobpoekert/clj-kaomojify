(ns clj-kaomojify.core
  (:gen-class)
  (require [opennlp.nlp :as onlp]
           [opennlp.tools.train :as train]
           [clojure.tools.reader.edn :as edn]
           [twitter.oauth :as oauth]
           [twitter.api.restful :as twtr]))

(def settings
  (edn/read-string (slurp "config.edn")))

(def twitter-creds
  (oauth/make-oauth-creds
    (:consumer-key settings)
    (:consumer-secret settings)
    (:access-token settings)
    (:access-token-secret settings)))

(def document-model
  (train/train-document-categorization "resources/emotions.tsv"))

(def categorizer
  (onlp/make-document-categorizer document-model))

(defn status-seq
  ([offset-id]
    (lazy-seq
      (let [default-params {:screen-name (:username settings)
                             :count "200"
                             :include-rts "false"
                             :trim-user "true"}
            response (twtr/statuses-user-timeline
                        :oauth-creds twitter-creds
                        :params (if offset-id
                                  (assoc default-params :max-id offset-id)
                                  default-params))
            body (:body response)]
        (if (empty? body)
          nil
          (concat body (status-seq (:id (last body))))))))
    ([] (status-seq nil)))

(defn get-statuses
  []
  (filter
    #(not (re-find #"(^|\s+)RT\s+" %))
    (map :text (status-seq))))

(defn moods
  []
  (map #(assoc (categorizer %) :tweet %) (get-statuses)))

(defn noisy-take
  ([total-n n s]
    (lazy-seq
      (if (or (empty? s) (zero? n)) nil
        (let [[head & tail] s
              percent (float (* 100 (/ (- total-n n) total-n)))]
          (if (= (Math/floor percent) percent)
            (println (str percent "%")))
          (cons head (noisy-take total-n (dec n) tail))))))
  ([n s]
    (noisy-take n n s)))

(defn current-mood
  []
  (let [all-moods (map :best-category (noisy-take 1000 (moods)))
        recent-moods (take 100 all-moods)
        pct-changes (merge-with
                      (fn [overall recent]
                        (/ (- recent overall) (* recent 100)))
                      (frequencies all-moods)
                      (frequencies recent-moods))]
      (first (apply max-key second (seq pct-changes)))))

(defn percentages
  [s]
  (let [freqs (frequencies s)
        total (reduce + (vals freqs))]
    (for [[k v] (seq freqs)]
      [k (double (* (/ v total) 100))])))

'(percentages (take 100 (map :best-category (moods))))


(defn current-kaomoji
  []
  (get (:kaomoji settings)
    (keyword (current-mood))))

(defn update-kaomoji
  []
  (let [status (str (:name settings) " " (current-kaomoji))]
    (twtr/account-update-profile
      :oauth-creds twitter-creds
      :params {:name status})
    status))

(defn -main
  [& args]
  (do
    (println (update-kaomoji))
    (System/exit 0)))
