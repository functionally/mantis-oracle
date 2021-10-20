#!/usr/bin/env bash

. env.sh

echo


### Altering data.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file $PROTOCOL \
  --tx-in $TXID_2#1 \
    --tx-in-script-file $SCRIPT_O \
    --tx-in-datum-value "$(cat $DATUM_0)" \
    --tx-in-redeemer-file ../redeemer-read.json \
  --tx-in $TXID_1#4 \
  --tx-in $TXID_2#0 \
  --tx-out "$ADDRESS_S+5000000+1 $TOKEN_D+10 $TOKEN_F" \
    --tx-out-datum-hash $HASH_1 \
  --tx-out "$ADDRESS_1+5000000+990 $TOKEN_F" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral $TXID_2#3 \
  --out-file tx.raw 2>/dev/null

assert_failure "02a No reading if altering datum."


### Discarding data.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file $PROTOCOL \
  --tx-in $TXID_2#1 \
    --tx-in-script-file $SCRIPT_O \
    --tx-in-datum-value "$(cat $DATUM_0)" \
    --tx-in-redeemer-file ../redeemer-read.json \
  --tx-in $TXID_1#4 \
  --tx-in $TXID_2#0 \
  --tx-out "$ADDRESS_S+5000000+1 $TOKEN_D+10 $TOKEN_F" \
  --tx-out "$ADDRESS_1+5000000+990 $TOKEN_F" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral $TXID_2#3 \
  --out-file tx.raw 2>/dev/null

assert_failure "02b No reading if discarding datum."


### Stealing datum token.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file $PROTOCOL \
  --tx-in $TXID_2#1 \
    --tx-in-script-file $SCRIPT_O \
    --tx-in-datum-value "$(cat $DATUM_0)" \
    --tx-in-redeemer-file ../redeemer-read.json \
  --tx-in $TXID_1#4 \
  --tx-in $TXID_2#0 \
  --tx-out "$ADDRESS_S+5000000+10 $TOKEN_F" \
  --tx-out "$ADDRESS_1+5000000+1 $TOKEN_D+990 $TOKEN_F" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral $TXID_2#3 \
  --out-file tx.raw 2>/dev/null

assert_failure "02c No reading if stealing datum token."


### Stealing everything.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file $PROTOCOL \
  --tx-in $TXID_2#1 \
    --tx-in-script-file $SCRIPT_O \
    --tx-in-datum-value "$(cat $DATUM_0)" \
    --tx-in-redeemer-file ../redeemer-read.json \
  --tx-in $TXID_1#4 \
  --tx-in $TXID_2#0 \
  --tx-out "$ADDRESS_1+5000000+1 $TOKEN_D+1000 $TOKEN_F" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral $TXID_2#3 \
  --out-file tx.raw 2>/dev/null

assert_failure "02d No reading if stealing everything."


### No fee.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file $PROTOCOL \
  --tx-in $TXID_2#1 \
    --tx-in-script-file $SCRIPT_O \
    --tx-in-datum-value "$(cat $DATUM_0)" \
    --tx-in-redeemer-file ../redeemer-read.json \
  --tx-in $TXID_1#4 \
  --tx-in $TXID_2#0 \
  --tx-out "$ADDRESS_S+5000000+1 $TOKEN_D" \
    --tx-out-datum-hash $HASH_0 \
  --tx-out "$ADDRESS_1+5000000+100 $TOKEN_F" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral $TXID_2#3 \
  --out-file tx.raw 2>/dev/null

assert_failure "02e No reading without any fee."


### Insufficient fee.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file $PROTOCOL \
  --tx-in $TXID_2#1 \
    --tx-in-script-file $SCRIPT_O \
    --tx-in-datum-value "$(cat $DATUM_0)" \
    --tx-in-redeemer-file ../redeemer-read.json \
  --tx-in $TXID_1#4 \
  --tx-in $TXID_2#0 \
  --tx-out "$ADDRESS_S+5000000+1 $TOKEN_D+9 $TOKEN_F" \
    --tx-out-datum-hash $HASH_0 \
  --tx-out "$ADDRESS_1+5000000+991 $TOKEN_F" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral $TXID_2#3 \
  --out-file tx.raw 2>/dev/null

assert_failure "02f No reading without sufficient fee."


### Excess fee.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file $PROTOCOL \
  --tx-in $TXID_2#1 \
    --tx-in-script-file $SCRIPT_O \
    --tx-in-datum-value "$(cat $DATUM_0)" \
    --tx-in-redeemer-file ../redeemer-read.json \
  --tx-in $TXID_1#4 \
  --tx-in $TXID_2#0 \
  --tx-out "$ADDRESS_S+5000000+1 $TOKEN_D+11 $TOKEN_F" \
    --tx-out-datum-hash $HASH_0 \
  --tx-out "$ADDRESS_1+5000000+989 $TOKEN_F" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral $TXID_2#3 \
  --out-file tx.raw 2>/dev/null

assert_failure "02g No reading with excess fee."


### Insufficient ADA.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file $PROTOCOL \
  --tx-in $TXID_2#1 \
    --tx-in-script-file $SCRIPT_O \
    --tx-in-datum-value "$(cat $DATUM_0)" \
    --tx-in-redeemer-file ../redeemer-read.json \
  --tx-in $TXID_1#4 \
  --tx-in $TXID_2#0 \
  --tx-out "$ADDRESS_S+4999999+1 $TOKEN_D+10 $TOKEN_F" \
    --tx-out-datum-hash $HASH_0 \
  --tx-out "$ADDRESS_1+5000000+990 $TOKEN_F" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral $TXID_2#3 \
  --out-file tx.raw 2>/dev/null

assert_failure "02h No reading without sufficient ADA."


### Excess ADA.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file $PROTOCOL \
  --tx-in $TXID_2#1 \
    --tx-in-script-file $SCRIPT_O \
    --tx-in-datum-value "$(cat $DATUM_0)" \
    --tx-in-redeemer-file ../redeemer-read.json \
  --tx-in $TXID_1#4 \
  --tx-in $TXID_2#0 \
  --tx-out "$ADDRESS_S+5000001+1 $TOKEN_D+10 $TOKEN_F" \
    --tx-out-datum-hash $HASH_0 \
  --tx-out "$ADDRESS_1+5000000+990 $TOKEN_F" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral $TXID_2#3 \
  --out-file tx.raw 2>/dev/null

assert_failure "02i No reading with excess ADA."


### Insufficent fee, excess ADA.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file $PROTOCOL \
  --tx-in $TXID_2#1 \
    --tx-in-script-file $SCRIPT_O \
    --tx-in-datum-value "$(cat $DATUM_0)" \
    --tx-in-redeemer-file ../redeemer-read.json \
  --tx-in $TXID_1#4 \
  --tx-in $TXID_2#0 \
  --tx-out "$ADDRESS_S+9999999+1 $TOKEN_D+9 $TOKEN_F" \
    --tx-out-datum-hash $HASH_0 \
  --tx-out "$ADDRESS_1+5000000+991 $TOKEN_F" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral $TXID_2#3 \
  --out-file tx.raw 2>/dev/null

assert_failure "02j No reading with insufficent fee and excess ADA."


### Insufficent ADA, excess fee.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file $PROTOCOL \
  --tx-in $TXID_2#1 \
    --tx-in-script-file $SCRIPT_O \
    --tx-in-datum-value "$(cat $DATUM_0)" \
    --tx-in-redeemer-file ../redeemer-read.json \
  --tx-in $TXID_1#4 \
  --tx-in $TXID_2#0 \
  --tx-out "$ADDRESS_S+4999999+1 $TOKEN_D+99 $TOKEN_F" \
    --tx-out-datum-hash $HASH_0 \
  --tx-out "$ADDRESS_1+5000000+901 $TOKEN_F" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral $TXID_2#3 \
  --out-file tx.raw 2>/dev/null

assert_failure "02k No reading with insufficent ADA and excess fee."


### Reading.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file $PROTOCOL \
  --tx-in $TXID_2#1 \
    --tx-in-script-file $SCRIPT_O \
    --tx-in-datum-value "$(cat $DATUM_0)" \
    --tx-in-redeemer-file ../redeemer-read.json \
  --tx-in $TXID_1#4 \
  --tx-in $TXID_2#0 \
  --tx-out "$ADDRESS_S+5000000+1 $TOKEN_D+10 $TOKEN_F" \
    --tx-out-datum-hash $HASH_0 \
  --tx-out "$ADDRESS_1+5000000+990 $TOKEN_F" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral $TXID_2#3 \
  --out-file tx.raw

assert_success "02l Reading."

cardano-cli transaction sign $MAGIC \
  --tx-body-file tx.raw \
  --out-file tx.signed \
  --signing-key-file $PAYMENT_1

cardano-cli transaction submit $MAGIC \
  --tx-file tx.signed

rm tx.{raw,signed}


### Record transaction.

echo "Run query.sh to find the resulting transaction and enter that as TXID_3 in local.sh."
