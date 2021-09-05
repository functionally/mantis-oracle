#!/usr/bin/env bash

. env.sh

echo


### Illegal redeemer.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file $PROTOCOL \
  --tx-in $TXID_4#1 \
    --tx-in-script-file $SCRIPT_O \
    --tx-in-datum-value "$(cat $DATUM_1)" \
    --tx-in-redeemer-value '3' \
  --tx-in $TXID_4#2 \
  --tx-in $TXID_4#0 \
  --tx-in $TXID_3#2 \
  --tx-out "$ADDRESS_S+5000000+1 $TOKEN_D+20 $TOKEN_F" \
    --tx-out-datum-hash $HASH_1 \
  --tx-out "$ADDRESS_1+5000000+1 $TOKEN_C" \
  --tx-out "$ADDRESS_1+5000000+980 $TOKEN_F" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral $TXID_2#3 \
  --out-file tx.raw 2>/dev/null

assert_failure "04a Illegal redeemer."


### Another illegal redeemer.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file $PROTOCOL \
  --tx-in $TXID_4#1 \
    --tx-in-script-file $SCRIPT_O \
    --tx-in-datum-value "$(cat $DATUM_1)" \
    --tx-in-redeemer-value '-1' \
  --tx-in $TXID_4#2 \
  --tx-in $TXID_4#0 \
  --tx-in $TXID_3#2 \
  --tx-out "$ADDRESS_S+5000000+1 $TOKEN_D+20 $TOKEN_F" \
    --tx-out-datum-hash $HASH_1 \
  --tx-out "$ADDRESS_1+5000000+1 $TOKEN_C" \
  --tx-out "$ADDRESS_1+5000000+980 $TOKEN_F" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral $TXID_2#3 \
  --out-file tx.raw 2>/dev/null

assert_failure "04b Another illegal redeemer."
