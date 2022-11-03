cardano-cli transaction build \
  --babbage-era \
  $PREPROD \
  --tx-in 1f0d45713071d897c74e6ddefceae390d71c87a0cdbf750cc864f006d006297f#0 \
  --tx-out $(cat mathBounty.addr)+500000000 \
  --tx-out-datum-hash-file datum.json \
  --change-address $Adr01 \
  --out-file tx.unsigned

cardano-cli transaction sign \
  --tx-body-file tx.unsigned \
  --signing-key-file Adr01.skey \
  $PREPROD \
  --out-file tx.signed

cardano-cli transaction submit \
  $PREPROD \
  --tx-file tx.signed