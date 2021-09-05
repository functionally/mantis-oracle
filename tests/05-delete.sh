#!/usr/bin/env bash

. env.sh

echo


### No deletion without control.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file $PROTOCOL \
  --tx-in $TXID_4#1 \
    --tx-in-script-file $SCRIPT_O \
    --tx-in-datum-value "$(cat $DATUM_1)" \
    --tx-in-redeemer-value '0' \
  --tx-in $TXID_4#0 \
  --tx-out "$ADDRESS_1+5000000+1 $TOKEN_D" \
  --tx-out "$ADDRESS_1+5000000+10 $TOKEN_F" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral $TXID_2#3 \
  --out-file tx.raw 2>/dev/null

assert_failure "05a No deletion without control."


### No deletion with fee instead of control.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file $PROTOCOL \
  --tx-in $TXID_4#1 \
    --tx-in-script-file $SCRIPT_O \
    --tx-in-datum-value "$(cat $DATUM_1)" \
    --tx-in-redeemer-value '0' \
  --tx-in $TXID_3#2 \
  --tx-out "$ADDRESS_1+5000000+1 $TOKEN_D" \
  --tx-out "$ADDRESS_1+5000000+990 $TOKEN_F" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral $TXID_2#3 \
  --out-file tx.raw 2>/dev/null

assert_failure "05b No deletion with fee instead of control."


### No deletion with ADA sent to script.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file $PROTOCOL \
  --tx-in $TXID_4#1 \
    --tx-in-script-file $SCRIPT_O \
    --tx-in-datum-value "$(cat $DATUM_1)" \
    --tx-in-redeemer-value '0' \
  --tx-in $TXID_3#0 \
  --tx-in $TXID_4#2 \
  --tx-out "$ADDRESS_S+5000000" \
  --tx-out "$ADDRESS_1+5000000+1 $TOKEN_C" \
  --tx-out "$ADDRESS_1+5000000+1 $TOKEN_D" \
  --tx-out "$ADDRESS_1+5000000+10 $TOKEN_F" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral $TXID_2#3 \
  --out-file tx.raw 2>/dev/null

assert_failure "05c No deletion with ADA sent to script."


### No deletion with datum token sent to script.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file $PROTOCOL \
  --tx-in $TXID_4#1 \
    --tx-in-script-file $SCRIPT_O \
    --tx-in-datum-value "$(cat $DATUM_1)" \
    --tx-in-redeemer-value '0' \
  --tx-in $TXID_3#0 \
  --tx-in $TXID_4#2 \
  --tx-out "$ADDRESS_1+5000000+1 $TOKEN_C" \
  --tx-out "$ADDRESS_S+5000000+1 $TOKEN_D" \
  --tx-out "$ADDRESS_1+5000000+10 $TOKEN_F" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral $TXID_2#3 \
  --out-file tx.raw 2>/dev/null

assert_failure "05d No deletion with datum token sent to script."


### No deletion with control token sent to script.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file $PROTOCOL \
  --tx-in $TXID_4#1 \
    --tx-in-script-file $SCRIPT_O \
    --tx-in-datum-value "$(cat $DATUM_1)" \
    --tx-in-redeemer-value '0' \
  --tx-in $TXID_3#0 \
  --tx-in $TXID_4#2 \
  --tx-out "$ADDRESS_S+5000000+1 $TOKEN_C" \
  --tx-out "$ADDRESS_1+5000000+1 $TOKEN_D" \
  --tx-out "$ADDRESS_1+5000000+10 $TOKEN_F" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral $TXID_2#3 \
  --out-file tx.raw 2>/dev/null

assert_failure "05e No deletion with control token sent to script."


### No deletion with control and datum tokens sent to script.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file $PROTOCOL \
  --tx-in $TXID_4#1 \
    --tx-in-script-file $SCRIPT_O \
    --tx-in-datum-value "$(cat $DATUM_1)" \
    --tx-in-redeemer-value '0' \
  --tx-in $TXID_3#0 \
  --tx-in $TXID_4#2 \
  --tx-out "$ADDRESS_S+5000000+1 $TOKEN_C" \
  --tx-out "$ADDRESS_S+5000000+1 $TOKEN_D" \
  --tx-out "$ADDRESS_1+5000000+10 $TOKEN_F" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral $TXID_2#3 \
  --out-file tx.raw 2>/dev/null

assert_failure "05f No deletion with control and datum tokens sent to script."


### Deletion.

mantra-oracle delete $CONFIG \
              $ADDRESS_1     \
              $PAYMENT_1     \
              $DATUM_1

assert_success "05g Deletion."


### Record transaction.

echo "Run query.sh to find the resulting transaction and enter that as TXID_5 in local.sh."
