(ns smoke.insta
  "Semi-automated Instagram Reels publishing via the Graph API.

   Dev-mode + your OWN account => NO app review needed (review is only for serving
   other users at scale). Creds live in a gitignored EDN file (default `.insta.edn`
   in the repo root):

     {:app-id     \"...\"
      :app-secret \"...\"
      :ig-user-id \"...\"          ; the Instagram BUSINESS account id
      :token      \"...\"}         ; a (long-lived) access token

   Per-reel flow (Meta fetches the video from a public URL — no direct upload):
     upload mp4 to a temp public host -> create REELS container -> poll until
     FINISHED -> publish -> read permalink -> write it into the <reel>.edn sidecar.

     (require '[smoke.insta :as ig])
     (ig/exchange-long-lived! )                       ; short token -> 60-day token
     (ig/post-reel! \"media/reels/brejcha-galaxy-slime-react-freq.mp4\")  ; caption from POSTING.md

   Limit: 25 published posts / 24 h. Uses embedded audio (no licensed-music picker
   via API) => copyright-mute risk, same as web/scheduled posting."
  (:require [clj-http.client :as http]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.pprint]))

(def ^:dynamic *creds-file* ".insta.edn")
(def ^:dynamic *api-version* "v21.0")   ; bump if Meta deprecates it

(defn creds []
  (let [f (io/file *creds-file*)]
    (when-not (.exists f)
      (throw (ex-info (str "creds file missing: " (.getPath f)) {})))
    (edn/read-string (slurp f))))

(defn- g-url [& parts]
  (str "https://graph.facebook.com/" *api-version* "/" (str/join "/" parts)))

(defn- gget [url params]
  (:body (http/get url {:query-params params :as :json :throw-exceptions true})))
(defn- gpost [url params]
  (:body (http/post url {:query-params params :as :json :throw-exceptions true})))

(defn exchange-long-lived!
  "Trade the short-lived :token for a ~60-day one and rewrite the creds file."
  ([] (exchange-long-lived! (creds)))
  ([{:keys [app-id app-secret token] :as c}]
   (let [tok (:access_token
              (gget (g-url "oauth/access_token")
                    {:grant_type "fb_exchange_token" :client_id app-id
                     :client_secret app-secret :fb_exchange_token token}))]
     (spit *creds-file* (with-out-str (clojure.pprint/pprint (assoc c :token tok))))
     (println "long-lived token written to" *creds-file*)
     tok)))

(defn upload-temp!
  "Upload a local file to 0x0.st; return its public direct URL (Meta fetches this)."
  [path]
  (let [r (http/post "https://0x0.st"
                     {:multipart [{:name "file" :content (io/file path)}]
                      :headers {"User-Agent" "smoke-viz/1.0 (reels)"}})]
    (str/trim (:body r))))

(defn create-container [{:keys [ig-user-id token]} video-url caption]
  (:id (gpost (g-url ig-user-id "media")
              {:media_type "REELS" :video_url video-url
               :caption caption :access_token token})))

(defn container-status [{:keys [token]} container-id]
  (:status_code (gget (g-url container-id)
                      {:fields "status_code" :access_token token})))

(defn wait-finished!
  "Poll the container until FINISHED (Reels need processing time)."
  [c container-id & {:keys [tries delay-ms] :or {tries 40 delay-ms 5000}}]
  (loop [n tries]
    (let [s (container-status c container-id)]
      (println "  container" container-id "status:" s)
      (cond
        (= s "FINISHED") :finished
        (= s "ERROR")    (throw (ex-info "container processing ERROR" {:id container-id}))
        (zero? n)        (throw (ex-info "timeout waiting for container" {:id container-id :status s}))
        :else (do (Thread/sleep (long delay-ms)) (recur (dec n)))))))

(defn publish [{:keys [ig-user-id token]} container-id]
  (:id (gpost (g-url ig-user-id "media_publish")
              {:creation_id container-id :access_token token})))

(defn permalink [{:keys [token]} media-id]
  (:permalink (gget (g-url media-id) {:fields "permalink" :access_token token})))

(defn caption-from-posting
  "Pull the fenced caption+hashtags block for `mp4-basename` out of POSTING.md."
  [posting-md mp4-basename]
  (let [txt (slurp posting-md)
        ;; section from this file's heading up to the next "### "
        sect (some-> (re-find (re-pattern (str "(?s)### "
                                               (java.util.regex.Pattern/quote mp4-basename)
                                               ".*?(?=\\n### |\\z)"))
                              txt))
        block (second (re-find #"(?s)```\n(.*?)\n```" (or sect "")))]
    (some-> block str/trim)))

(defn- write-sidecar-posted! [mp4 permalink-url]
  (let [edn-file (str mp4 ".edn")
        data (edn/read-string (slurp edn-file))
        stamp (str (java.time.Instant/now))
        data' (update data :insta merge {:posted-url permalink-url :posted-at stamp})]
    (spit edn-file (with-out-str (clojure.pprint/pprint data')))
    edn-file))

(defn post-reel!
  "Publish one reel mp4 to Instagram and record the permalink in its sidecar.
   Caption: explicit `:caption`, else pulled from POSTING.md by basename."
  [mp4 & {:keys [caption posting-md] :or {posting-md "media/reels/POSTING.md"}}]
  (let [c    (creds)
        base (.getName (io/file mp4))
        cap  (or caption (caption-from-posting posting-md base)
                 (throw (ex-info "no caption (give :caption or add to POSTING.md)" {:mp4 mp4})))]
    (println "uploading" mp4 "...")
    (let [url (upload-temp! mp4)
          _   (println "  public url:" url)
          cid (create-container c url cap)
          _   (println "  container:" cid)]
      (wait-finished! c cid)
      (let [mid  (publish c cid)
            link (permalink c mid)]
        (write-sidecar-posted! mp4 link)
        (println "PUBLISHED:" link)
        {:mp4 mp4 :media-id mid :permalink link}))))

(comment
  (require '[smoke.insta :as ig] :reload)
  (ig/exchange-long-lived!)                  ; once, after pasting a short token
  (ig/post-reel! "media/reels/brejcha-galaxy-slime-react-freq.mp4")
  ;; check status of a stuck container:
  (ig/container-status (ig/creds) "<container-id>"))
