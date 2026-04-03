(ns schema.api
  (:require [malli.core :as m]
            [malli.transform :as mt]))

;; Enums and Normalizers

(def SubscriptionState
  [:enum
   1 ;; UNSUBSCRIBED
   2 ;; RECURRING
   3 ;; NON_RECURRING
   4 ;; PENDING_ACTIVATION
   5]) ;; DECLINED

(def SubscriptionTier
  [:enum
   0 ;; NONE
   1 ;; PRO
   2]) ;; PRO_PLUS

(def ColabSubscriptionTier
  [:enum 0 1 2])

(def ColabGapiSubscriptionTier
  [:enum
   "SUBSCRIPTION_TIER_UNSPECIFIED"
   "SUBSCRIPTION_TIER_NONE"
   "SUBSCRIPTION_TIER_PRO"
   "SUBSCRIPTION_TIER_PRO_PLUS"])

(defn normalize-sub-tier [tier]
  (cond
    (or (= tier 1) (= tier "SUBSCRIPTION_TIER_PRO")) 1 ;; PRO
    (or (= tier 2) (= tier "SUBSCRIPTION_TIER_PRO_PLUS")) 2 ;; PRO_PLUS
    :else 0)) ;; NONE

(def Outcome
  [:enum
   0 ;; UNDEFINED_OUTCOME
   1 ;; QUOTA_DENIED_REQUESTED_VARIANTS
   2 ;; QUOTA_EXCEEDED_USAGE_TIME
   4 ;; SUCCESS
   5]) ;; DENYLISTED

(def Variant
  [:enum "DEFAULT" "GPU" "TPU"])

(def ColabGapiVariant
  [:enum "VARIANT_UNSPECIFIED" "VARIANT_GPU" "VARIANT_TPU"])

(defn normalize-variant [variant]
  (case variant
    "VARIANT_GPU" "GPU"
    "VARIANT_TPU" "TPU"
    "DEFAULT"))

(def Shape
  [:enum
   0 ;; STANDARD
   1]) ;; HIGHMEM

(def ColabGapiShape
  [:enum "SHAPE_UNSPECIFIED" "SHAPE_DEFAULT" "SHAPE_HIGH_MEM"])

(defn normalize-shape [shape]
  (case shape
    "SHAPE_HIGH_MEM" 1 ;; HIGHMEM
    0)) ;; STANDARD

(def AuthType
  [:enum "dfs_ephemeral" "auth_user_ephemeral"])

;; Schemas

(def Accelerator
  [:map
   [:variant {:decode/json normalize-variant} ColabGapiVariant]
   [:models {:optional true
             :decode/json #(or % [])}
    [:vector :string]]])

(def UserInfo
  [:map
   [:subscriptionTier {:decode/json normalize-sub-tier}
    [:or ColabSubscriptionTier ColabGapiSubscriptionTier]]
   [:paidComputeUnitsBalance {:optional true} :double]
   [:eligibleAccelerators [:vector Accelerator]]
   [:ineligibleAccelerators [:vector Accelerator]]])

(def ConsumptionUserInfo
  (m/schema
   [:map
    [:subscriptionTier {:decode/json normalize-sub-tier}
     [:or ColabSubscriptionTier ColabGapiSubscriptionTier]]
    [:paidComputeUnitsBalance :double]
    [:eligibleAccelerators [:vector Accelerator]]
    [:ineligibleAccelerators [:vector Accelerator]]
    [:consumptionRateHourly :double]
    [:assignmentsCount :double]
    [:freeCcuQuotaInfo {:optional true}
     [:map
      [:remainingTokens {:decode/json #(if (string? %) (parse-double %) %)} :double]
      [:nextRefillTimestampSec :double]]]]))

(def GetAssignmentResponse
  [:map
   [:acc :string]
   [:nbh :string]
   [:p :boolean]
   [:token :string]
   [:variant Variant]])

(def RuntimeProxyInfo
  [:map
   [:token :string]
   [:tokenExpiresInSeconds :double]
   [:url :string]])

(def DEFAULT_TOKEN_TTL_SECONDS 3600)

(defn parse-token-ttl [token-ttl]
  (let [secs (if (string? token-ttl)
               (try (parse-double (subs token-ttl 0 (dec (count token-ttl))))
                    (catch Exception _ DEFAULT_TOKEN_TTL_SECONDS))
               token-ttl)]
    (if (or (nil? secs) (Double/isNaN secs) (<= secs 0))
      DEFAULT_TOKEN_TTL_SECONDS
      secs)))

(def RuntimeProxyToken
  [:map
   [:token :string]
   [:tokenTtl :string]
   [:url :string]])

(def PostAssignmentResponse
  [:map
   [:accelerator {:optional true} :string]
   [:endpoint {:optional true} :string]
   [:fit {:optional true} :double]
   [:allowedCredentials {:optional true} :boolean]
   [:sub {:optional true} SubscriptionState]
   [:subTier {:optional true :decode/json normalize-sub-tier}
    [:or ColabSubscriptionTier ColabGapiSubscriptionTier]]
   [:outcome {:optional true} Outcome]
   [:variant {:optional true :decode/json #(if (number? %)
                                             (case % 0 "DEFAULT" 1 "GPU" 2 "TPU" "DEFAULT")
                                             %)}
    Variant]
   [:machineShape {:optional true} Shape]
   [:runtimeProxyInfo {:optional true} RuntimeProxyInfo]])

(def ListedAssignment
  [:map
   [:endpoint :string]
   [:accelerator :string]
   [:variant {:decode/json normalize-variant} ColabGapiVariant]
   [:machineShape {:decode/json normalize-shape} ColabGapiShape]
   [:runtimeProxyInfo {:optional true} RuntimeProxyToken]])

(def ListedAssignments
  [:map
   [:assignments {:optional true :decode/json #(or % [])}
    [:vector ListedAssignment]]])

(def Assignment
  [:map
   [:accelerator :string]
   [:endpoint :string]
   [:fit {:optional true} :double]
   [:allowedCredentials {:optional true} :boolean]
   [:sub {:optional true} SubscriptionState]
   [:subTier {:optional true :decode/json normalize-sub-tier}
    [:or ColabSubscriptionTier ColabGapiSubscriptionTier]]
   [:variant Variant]
   [:machineShape Shape]
   [:runtimeProxyInfo RuntimeProxyInfo]])

(def Kernel
  [:map
   [:id :string]
   [:name :string]
   [:last_activity :string]
   [:execution_state :string]
   [:connections :double]])

(def Session
  [:map
   [:id :string]
   [:kernel Kernel]
   [:name :string]
   [:path :string]
   [:type :string]])

(def Memory
  [:map
   [:totalBytes {:optional true :decode/json #(or % 0)} :double]
   [:freeBytes {:optional true :decode/json #(or % 0)} :double]])

(def GpuInfo
  [:map
   [:name {:optional true} :string]
   [:memoryUsedBytes {:optional true :decode/json #(or % 0)} :double]
   [:memoryTotalBytes {:optional true :decode/json #(or % 0)} :double]
   [:gpuUtilization {:optional true} :double]
   [:memoryUtilization {:optional true} :double]
   [:everUsed {:optional true} :boolean]])

(def Filesystem
  [:map
   [:label {:optional true} :string]
   [:totalBytes {:optional true :decode/json #(or % 0)} :double]
   [:usedBytes {:optional true :decode/json #(or % 0)} :double]])

(def Disk
  [:map
   [:filesystem {:optional true :decode/json #(or % {:totalBytes 0 :usedBytes 0})}
    Filesystem]])

(def Resources
  [:map
   [:memory {:optional true :decode/json #(or % {:totalBytes 0 :freeBytes 0})}
    Memory]
   [:disks [:vector Disk]]
   [:gpus {:optional true :decode/json #(or % [])}
    [:vector GpuInfo]]])

(def CredentialsPropagationResult
  [:map
   [:success :boolean]
   [:unauthorized_redirect_uri {:optional true} :string]])

(defn is-high-mem-only-accelerator? [accelerator]
  (contains? #{"L4" "V28" "V5E1" "V6E1"} accelerator))

(def ExperimentFlagValue
  [:or :string :double :boolean [:vector [:or :string :double :boolean]]])

(def ExperimentState
  [:map
   [:experiments {:optional true}
    [:map-of :string ExperimentFlagValue]]])
