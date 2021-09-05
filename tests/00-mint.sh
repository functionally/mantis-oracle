#!/usr/bin/env bash

set -e

. env.sh


### Mint and distribute the tokens.

cardano-cli transaction build $MAGIC --alonzo-era \
  --tx-in $TXID_0#0 \
  --tx-out "$ADDRESS_1+100000000" \
  --tx-out "$ADDRESS_1+100000000+1    $TOKEN_C" \
  --tx-out "$ADDRESS_1+100000000+1    $TOKEN_D" \
  --tx-out "$ADDRESS_1+100000000+1000 $TOKEN_F" \
  --tx-out "$ADDRESS_1+2000000" \
  --change-address $ADDRESS_0 \
  --mint "1 $TOKEN_C+1 $TOKEN_D+1000 $TOKEN_F" \
  --mint-script-file $SCRIPT_M \
  --out-file tx.raw

cardano-cli transaction sign $MAGIC \
  --tx-body-file tx.raw \
  --out-file tx.signed \
  --signing-key-file $PAYMENT_0

cardano-cli transaction submit $MAGIC \
  --tx-file tx.signed

rm tx.{raw,signed}


### Record transaction.

echo "Run query.sh to find the resulting transaction and enter that as TXID_1 in local.sh."
