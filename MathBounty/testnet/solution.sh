cardano-cli transaction build \
  --babbage-era \
  $PREVIEW \
  --tx-in 35d4acfe8b1fdf06a4842152b811ff7bd0e9e95077132a00937945293bae0feb#1 \
  --tx-in-script-file ./mathBounty.plutus \
  --tx-in-datum-file ./datum.json  \
  --tx-in-redeemer-file ./goodRedeemer.json \
  --required-signer-hash 8fd2af318fe6fd7a8b2f56861b7dda312411281616b902953abf7121 \
  --tx-in-collateral 509338748525e597f5dffe00b11944a53db75e48d53b29dd6b7e5bbe5e1eb40c#0 \
  --tx-out $Adr01+44000000 \
  --change-address $nami \
  --invalid-hereafter 4522475 \
  --protocol-params-file ./protocol.params \
  --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file Adr01.skey \
    $PREVIEW \
    --out-file tx.signed

cardano-cli transaction submit \
    $PREVIEW \
    --tx-file tx.signed