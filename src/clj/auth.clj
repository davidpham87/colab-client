(ns auth
  (:require [clojure.string :as str]
            [cheshire.core :as json]
            [hato.client :as hc]
            [org.httpkit.server :as server])
  (:import [java.net URLDecoder URLEncoder ServerSocket URI]
           [java.security SecureRandom MessageDigest]
           [java.util Base64]))

;; --- Configuration ---

(def client-id "YOUR_CLIENT_ID") ;; Since this is for Babashka CLI script, they might need to plug in their client-id/secret or Google provides native app ones.
(def client-secret "YOUR_CLIENT_SECRET")
(def scopes ["https://www.googleapis.com/auth/userinfo.email"
             "https://www.googleapis.com/auth/userinfo.profile"
             "https://www.googleapis.com/auth/drive"
             "https://www.googleapis.com/auth/colab"])

(def auth-endpoint "https://accounts.google.com/o/oauth2/v2/auth")
(def token-endpoint "https://oauth2.googleapis.com/token")

(defonce auth-token (atom nil))

;; --- Helpers ---

(defn generate-secure-random-string [length]
  (let [secure-random (SecureRandom.)
        bytes (byte-array length)]
    (.nextBytes secure-random bytes)
    (.encodeToString (Base64/getUrlEncoder) bytes)))

(defn generate-pkce []
  (let [verifier (generate-secure-random-string 32)
        md (MessageDigest/getInstance "SHA-256")
        _ (.update md (.getBytes verifier "US-ASCII"))
        digest (.digest md)
        challenge (-> (Base64/getUrlEncoder)
                      (.withoutPadding)
                      (.encodeToString digest))]
    {:verifier verifier
     :challenge challenge}))

(defn url-encode [s]
  (URLEncoder/encode s "UTF-8"))

(defn open-browser [url]
  (println "Please open this URL in your browser:")
  (println url))

(defn get-free-port []
  (with-open [socket (ServerSocket. 0)]
    (.getLocalPort socket)))

(defn parse-query-string [query]
  (if (empty? query)
    {}
    (->> (str/split query #"&")
         (map #(str/split % #"=" 2))
         (map (fn [[k v]] [k (URLDecoder/decode (or v "") "UTF-8")]))
         (into {}))))

(defn extract-query-from-url [url-str]
  (let [query-idx (str/index-of url-str "?")]
    (if query-idx
      (parse-query-string (subs url-str (inc query-idx)))
      {})))

;; --- Loopback Server & Flow ---

(defn wait-for-auth [port state-nonce redirect-uri]
  (let [result-promise (promise)
        stop-server-fn (atom nil)
        handler (fn [req]
                  (let [uri (:uri req)
                        query-params (extract-query-from-url (:query-string req ""))]
                    (cond
                      (= uri "/")
                      (let [state (:state query-params)
                            code (:code query-params)
                            error (:error query-params)]
                        (if error
                          (do
                            (deliver result-promise {:error error})
                            {:status 400
                             :headers {"Content-Type" "text/plain"}
                             :body "Authorization failed. You can close this window."})
                          (if (and state (str/includes? state state-nonce) code)
                            (do
                              (deliver result-promise {:code code})
                              {:status 200
                               :headers {"Content-Type" "text/plain"}
                               :body "Authorization successful! You can close this window and return to the terminal."})
                            (do
                              (deliver result-promise {:error "Invalid state or missing code."})
                              {:status 400
                               :headers {"Content-Type" "text/plain"}
                               :body "Authorization failed due to state mismatch. You can close this window."}))))
                      :else
                      {:status 404
                       :headers {"Content-Type" "text/plain"}
                       :body "Not found"})))]
    (reset! stop-server-fn (server/run-server handler {:port port}))
    (try
      (let [result @result-promise]
        (@stop-server-fn :timeout 100)
        result)
      (catch Exception e
        (@stop-server-fn :timeout 100)
        (throw e)))))

(defn exchange-code-for-token [code redirect-uri pkce-verifier]
  (let [body (->> {"code" code
                   "client_id" client-id
                   "client_secret" client-secret
                   "redirect_uri" redirect-uri
                   "grant_type" "authorization_code"
                   "code_verifier" pkce-verifier}
                  (map (fn [[k v]] (str (url-encode k) "=" (url-encode v))))
                  (str/join "&"))
        resp (hc/post token-endpoint
                      {:headers {"Content-Type" "application/x-www-form-urlencoded"}
                       :body body})]
    (if (= 200 (:status resp))
      (json/parse-string (:body resp) true)
      (throw (ex-info "Failed to exchange token" {:response (:body resp)})))))

(defn login []
  (let [port (get-free-port)
        redirect-uri (str "http://127.0.0.1:" port)
        nonce (generate-secure-random-string 16)
        pkce (generate-pkce)
        auth-url (str auth-endpoint
                      "?client_id=" (url-encode client-id)
                      "&redirect_uri=" (url-encode redirect-uri)
                      "&response_type=code"
                      "&scope=" (url-encode (str/join " " scopes))
                      "&state=" (url-encode (str "nonce=" nonce))
                      "&code_challenge=" (:challenge pkce)
                      "&code_challenge_method=S256"
                      "&prompt=consent")]
    (println "Starting local server on port" port "to listen for OAuth redirect...")
    (open-browser auth-url)
    (let [result (wait-for-auth port nonce redirect-uri)]
      (if (:code result)
        (let [token-info (exchange-code-for-token (:code result) redirect-uri (:verifier pkce))]
          (reset! auth-token token-info)
          (println "Successfully authenticated!")
          token-info)
        (throw (ex-info "Authentication failed" result))))))

(defn get-access-token []
  (when-not @auth-token
    (println "No access token found. Initiating login flow...")
    (login))
  (:access_token @auth-token))
