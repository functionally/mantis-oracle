#!/usr/bin/env bash

. env.sh

echo


### Burn tokens and collect ADA.

cardano-cli transaction build $MAGIC --alonzo-era \
  --tx-in $TXID_1#0 \
  --tx-in $TXID_3#2 \
  --tx-in $TXID_5#0 \
  --tx-in $TXID_5#1 \
  --tx-in $TXID_5#2 \
  --tx-in $TXID_5#3 \
  --change-address $ADDRESS_0 \
  --mint "-1 $TOKEN_C+-1 $TOKEN_D+-1000 $TOKEN_F" \
  --mint-script-file $SCRIPT_M \
  --out-file tx.raw

cardano-cli transaction sign $MAGIC \
  --tx-body-file tx.raw \
  --out-file tx.signed \
  --signing-key-file $PAYMENT_0 \
  --signing-key-file $PAYMENT_1

cardano-cli transaction submit $MAGIC \
  --tx-file tx.signed

rm tx.{raw,signed}
