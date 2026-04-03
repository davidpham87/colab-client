(ns colab.client
  (:require [auth]
            [schema.api :as api]
            [malli.core :as m]
            [malli.transform :as mt]
            [malli.error :as me]
            [hato.client :as hc]
            [cheshire.core :as json]
            [clojure.string :as str]))

;; --- Configuration ---

(def colab-domain "https://colab.research.google.com")
(def colab-gapi-domain "https://colab.research.google.com")
(def tun-endpoint "/tun/m")

(def default-headers
  {"Accept" "application/json"
   "X-Colab-Client-Agent" "vscode"
   "X-Colab-VS-Code-App-Name" "Visual Studio Code"
   "X-Colab-VS-Code-Extension-Version" "0.5.0"})

(def json-transformer (mt/transformer mt/json-transformer mt/strip-extra-keys-transformer))

;; --- Exceptions ---

(defn throw-api-error [msg status response]
  (throw (ex-info msg {:status status :response response})))

(defn throw-too-many-assignments [msg]
  (throw (ex-info msg {:type :too-many-assignments})))

(defn throw-accelerator-unavailable [accelerator]
  (throw (ex-info (str "Requested accelerator \"" accelerator "\" is unavailable")
                  {:type :accelerator-unavailable})))

(defn throw-denylisted [msg]
  (throw (ex-info msg {:type :denylisted})))

(defn throw-insufficient-quota [msg]
  (throw (ex-info msg {:type :insufficient-quota})))

;; --- Request Issuer ---

(defn- build-url [base-url path params]
  (let [query-string (->> params
                          (filter (fn [[_ v]] (some? v)))
                          (map (fn [[k v]] (str (name k) "=" (java.net.URLEncoder/encode (str v) "UTF-8"))))
                          (str/join "&"))
        full-url (if (str/starts-with? path "http")
                   path
                   (str base-url (if (str/starts-with? path "/") "" "/") path))]
    (if (empty? query-string)
      full-url
      (str full-url "?" query-string))))

(defn issue-request
  [{:keys [method path base-domain params headers with-auth? schema]
    :or {method :get
         base-domain colab-domain
         params {}
         headers {}
         with-auth? true}}]
  (let [auth-token (if with-auth? (auth/get-access-token) nil)
        final-params (if (= base-domain colab-domain)
                       (assoc params :authuser "0")
                       params)
        url (build-url base-domain path final-params)
        final-headers (merge default-headers
                             headers
                             (when auth-token {"Authorization" (str "Bearer " auth-token)}))
        request-opts {:method method
                      :url url
                      :headers final-headers
                      :throw-exceptions false}
        response (hc/request request-opts)
        status (:status response)
        body-str (:body response)]
    (if (and (>= status 200) (< status 300))
      (if schema
        (let [parsed-json (json/parse-string body-str true)
              decoded (m/decode schema parsed-json json-transformer)]
          (if (m/validate schema decoded)
            decoded
            (throw (ex-info "Response schema validation failed"
                            {:errors (me/humanize (m/explain schema decoded))
                             :response parsed-json}))))
        nil)
      (throw-api-error (str "Request failed with status " status) status body-str))))

;; --- Client API ---

(defn get-user-info []
  (issue-request {:method :get
                  :path "v1/user-info"
                  :base-domain colab-gapi-domain
                  :schema api/UserInfo}))

(defn get-consumption-user-info []
  (issue-request {:method :get
                  :path "v1/user-info"
                  :base-domain colab-gapi-domain
                  :params {:get_ccu_consumption_info "true"}
                  :schema api/ConsumptionUserInfo}))

(defn uuid->websafe-base64 [uuid-str]
  (let [uuid (java.util.UUID/fromString uuid-str)
        bb (java.nio.ByteBuffer/wrap (byte-array 16))]
    (.putLong bb (.getMostSignificantBits uuid))
    (.putLong bb (.getLeastSignificantBits uuid))
    (-> (java.util.Base64/getUrlEncoder)
        (.withoutPadding)
        (.encodeToString (.array bb)))))

(defn- build-assign-url [notebook-hash {:keys [variant accelerator shape version]}]
  (let [params (cond-> {:nbh (uuid->websafe-base64 notebook-hash)}
                 (and variant (not= variant "DEFAULT")) (assoc :variant variant)
                 accelerator (assoc :accelerator accelerator)
                 version (assoc :runtime_version_label version))
        final-shape (if (api/is-high-mem-only-accelerator? accelerator)
                      1
                      (or shape 0))]
    (if (= final-shape 1)
      (assoc params :shape "hm")
      params)))

(defn get-assignment [notebook-hash params]
  (let [query-params (build-assign-url notebook-hash params)
        response (issue-request {:method :get
                                 :path (str tun-endpoint "/assign")
                                 :base-domain colab-domain
                                 :params query-params
                                 :schema [:or api/GetAssignmentResponse api/Assignment]})]
    (if (:token response)
      (assoc response :kind "to_assign")
      (assoc response :kind "assigned"))))

(defn post-assignment [notebook-hash xsrf-token params]
  (let [query-params (build-assign-url notebook-hash params)]
    (try
      (issue-request {:method :post
                      :path (str tun-endpoint "/assign")
                      :base-domain colab-domain
                      :params query-params
                      :headers {"X-Goog-Colab-Token" xsrf-token}
                      :schema api/PostAssignmentResponse})
      (catch clojure.lang.ExceptionInfo e
        (let [status (:status (ex-data e))]
          (cond
            (= status 412) (throw-too-many-assignments (.getMessage e))
            (= status 503) (throw-accelerator-unavailable (or (:accelerator params) "default"))
            :else (throw e)))))))

(defn assign [notebook-hash params]
  (let [assignment (get-assignment notebook-hash params)]
    (if (= (:kind assignment) "assigned")
      {:assignment (dissoc assignment :kind)
       :is-new false}
      (let [res (post-assignment notebook-hash (:token assignment) params)
            outcome (:outcome res)]
        (cond
          (or (= outcome 1) (= outcome 2))
          (throw-insufficient-quota "You have insufficient quota to assign this server.")

          (= outcome 5)
          (throw-denylisted "This account has been blocked from accessing Colab servers due to suspected abusive activity.")

          :else
          (let [valid-assignment (m/decode api/Assignment res json-transformer)]
             {:assignment valid-assignment
              :is-new true}))))))

(defn unassign [endpoint]
  (let [url (str tun-endpoint "/unassign/" endpoint)
        token-res (issue-request {:method :get
                                  :path url
                                  :base-domain colab-domain
                                  :schema [:map [:token :string]]})]
    (issue-request {:method :post
                    :path url
                    :base-domain colab-domain
                    :headers {"X-Goog-Colab-Token" (:token token-res)}})
    nil))

(defn refresh-connection [endpoint]
  (issue-request {:method :get
                  :path "v1/runtime-proxy-token"
                  :base-domain colab-gapi-domain
                  :params {:endpoint endpoint :port "8080"}
                  :schema api/RuntimeProxyToken}))

(defn list-assignments []
  (let [response (issue-request {:method :get
                                 :path "v1/assignments"
                                 :base-domain colab-gapi-domain
                                 :schema api/ListedAssignments})]
    (or (:assignments response) [])))

(defn list-sessions [endpoint]
  (issue-request {:method :get
                  :path (str tun-endpoint "/" endpoint "/api/sessions")
                  :base-domain colab-domain
                  :headers {"X-Colab-Tunnel" "Google"}
                  :schema [:vector api/Session]}))

(defn get-resources [server-base-url runtime-proxy-token]
  (issue-request {:method :get
                  :path "api/colab/resources"
                  :base-domain server-base-url
                  :headers {"X-Colab-Runtime-Proxy-Token" runtime-proxy-token}
                  :schema api/Resources}))

(defn propagate-credentials [endpoint auth-type dry-run]
  (let [url (str tun-endpoint "/credentials-propagation/" endpoint)
        params {:authtype auth-type
                :version "2"
                :dryrun (str dry-run)
                :propagate "true"
                :record "false"}
        token-res (issue-request {:method :get
                                  :path url
                                  :base-domain colab-domain
                                  :params params
                                  :schema [:map [:token :string]]})]
    (issue-request {:method :post
                    :path url
                    :base-domain colab-domain
                    :params params
                    :headers {"X-Goog-Colab-Token" (:token token-res)}
                    :schema api/CredentialsPropagationResult})))

(defn send-keep-alive [endpoint]
  (issue-request {:method :get
                  :path (str tun-endpoint "/" endpoint "/keep-alive/")
                  :base-domain colab-domain
                  :headers {"X-Colab-Tunnel" "Google"}})
  nil)

(defn get-experiment-state
  ([] (get-experiment-state false))
  ([with-auth?]
   (issue-request {:method :get
                   :path "vscode/experiment-state"
                   :base-domain colab-domain
                   :with-auth? with-auth?
                   :schema api/ExperimentState})))
