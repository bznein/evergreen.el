(setq json_evg (shell-command-to-string "yq eval -j /Users/nikolas.de-giorgis/go/src/github.com/10gen/ops-manager-kubernetes/.evergreen.yml"))
(setq dict (json-read-from-string json_evg))


;; build build_variants name lists
(setq variants())
(loop for bv across (assoc-default 'buildvariants dict) do
      (progn
        (push (assoc-default 'name bv) variants
        )
        )
      )
(message "%s" variants)
