#!/usr/bin/env bash

. env.sh

echo


### No control.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file $PROTOCOL \
  --tx-in $TXID_3#1 \
    --tx-in-script-file $SCRIPT_O \
    --tx-in-datum-value "$(cat $DATUM_0)" \
    --tx-in-redeemer-file ../redeemer-write.json \
  --tx-in $TXID_3#0 \
  --tx-out "$ADDRESS_S+5000000+1 $TOKEN_D+10 $TOKEN_F" \
    --tx-out-datum-hash $HASH_1 \
  --tx-out "$ADDRESS_1+5000000" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral $TXID_2#3 \
  --out-file tx.raw 2>/dev/null

assert_failure "03a No writing without control."


### Stealing datum token.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file $PROTOCOL \
  --tx-in $TXID_3#1 \
    --tx-in-script-file $SCRIPT_O \
    --tx-in-datum-value "$(cat $DATUM_0)" \
    --tx-in-redeemer-file ../redeemer-write.json \
  --tx-in $TXID_2#2 \
  --tx-in $TXID_3#0 \
  --tx-out "$ADDRESS_S+5000000+10 $TOKEN_F" \
    --tx-out-datum-hash $HASH_1 \
  --tx-out "$ADDRESS_1+5000000+1 $TOKEN_C+1 $TOKEN_D" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral $TXID_2#3 \
  --out-file tx.raw 2>/dev/null

assert_failure "03b No writing if stealing datum token."


### Stealing everything.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file $PROTOCOL \
  --tx-in $TXID_3#1 \
    --tx-in-script-file $SCRIPT_O \
    --tx-in-datum-value "$(cat $DATUM_0)" \
    --tx-in-redeemer-file ../redeemer-write.json \
  --tx-in $TXID_2#2 \
  --tx-in $TXID_3#0 \
  --tx-out "$ADDRESS_1+5000000+1 $TOKEN_C+1 $TOKEN_D+10 $TOKEN_F" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral $TXID_2#3 \
  --out-file tx.raw 2>/dev/null

assert_failure "03c No writing if stealing everything."


### Depositing control token.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file $PROTOCOL \
  --tx-in $TXID_3#1 \
    --tx-in-script-file $SCRIPT_O \
    --tx-in-datum-value "$(cat $DATUM_0)" \
    --tx-in-redeemer-file ../redeemer-write.json \
  --tx-in $TXID_2#2 \
  --tx-in $TXID_3#0 \
  --tx-out "$ADDRESS_S+5000000+1 $TOKEN_C+1 $TOKEN_D+10 $TOKEN_F" \
    --tx-out-datum-hash $HASH_1 \
  --tx-out "$ADDRESS_1+5000000" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral $TXID_2#3 \
  --out-file tx.raw 2>/dev/null

assert_failure "03d No writing if depositing control token."


### Writing.

mantra-oracle write $CONFIG \
              $ADDRESS_1    \
              $PAYMENT_1    \
              $DATUM_0      \
              $DATUM_1

assert_success "03e Writing."


### Record transaction.

echo "Run query.sh to find the resulting transaction and enter that as TXID_4 in local.sh."
