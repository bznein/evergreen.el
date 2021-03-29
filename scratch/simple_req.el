(require 'request)

(request
 "https://evergreen.mongodb.com/rest/v2/tasks/mongodb_kubernetes_operator_e2e_tests_e2e_test_feature_compatibility_version_13eb844c55774ce8a6de51edde1a66b4371f3ef6_21_03_25_15_57_45"
 :headers '(("Api-User" . "nikolas.de-giorgis") ("Api-Key" . "xxxxx"))'
 :parser 'json-read
 :success (cl-function
 (lambda (&key data &allow-other-keys)
             (message "I sent: %S" data ))))
