MAGIC="--testnet-magic 8"


ADDRESS_0=$(cat keys/alonzo-purple.payment-0.address); echo $ADDRESS_0

ADDRESS_1=$(cat keys/alonzo-purple.payment-1.address); echo $ADDRESS_1

CURRENCY=13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1

DATUM='"Hello PIGY!"'; echo $DATUM
DATUM_HASH=$(cardano-cli transaction hash-script-data --script-data-value '"Hello PIGY!"'); echo $DATUM_HASH


SCRIPT_FILE=/scratch/code.functionally.io/sofr-oracle/repo/oracle.plutus; echo $SCRIPT_FILE
ADDRESS_S=$(cardano-cli address build $MAGIC --payment-script-file $SCRIPT_FILE); echo $ADDRESS_S


cardano-cli query utxo $MAGIC --address $ADDRESS_S
cardano-cli query utxo $MAGIC --address $ADDRESS_1

cardano-cli transaction build $MAGIC --alonzo-era \
  --tx-in 8f354c6f67ca667677abb04ac1c3810c17f97235debbb3e7eb3496f6fdc16a3d#2 \
  --tx-in 8f354c6f67ca667677abb04ac1c3810c17f97235debbb3e7eb3496f6fdc16a3d#0 \
  --tx-out "$ADDRESS_S+5000000+1 $CURRENCY.tSOFR" \
    --tx-out-datum-hash $DATUM_HASH \
  --tx-out "$ADDRESS_1+5000000+92 $CURRENCY.tSOFR" \
  --change-address $ADDRESS_1 \
  --out-file tx.raw

cardano-cli transaction sign $MAGIC \
  --tx-body-file tx.raw \
  --out-file tx.signed \
  --signing-key-file keys/alonzo-purple.payment-1.skey

cardano-cli transaction submit $MAGIC \
  --tx-file tx.signed


cardano-cli query utxo $MAGIC --address $ADDRESS_S
cardano-cli query utxo $MAGIC --address $ADDRESS_1

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file alonzo-purple.protocol \
  --tx-in 19c2a6bcedcf6ce8550c973537ae595ffdf4cea32b748cf0f08fa1667b3772bf#1 \
    --tx-in-script-file $SCRIPT_FILE \
    --tx-in-datum-value '"Hello PIGY!"' \
    --tx-in-redeemer-value '1' \
  --tx-in aec4576aeaa7e394345d714c08a0205649bf3bec44544e19277d596a09b45a21#3 \
  --tx-in 19c2a6bcedcf6ce8550c973537ae595ffdf4cea32b748cf0f08fa1667b3772bf#0 \
  --tx-out "$ADDRESS_S+5000000+1 $CURRENCY.tSOFR+10 $CURRENCY.tPIGY" \
    --tx-out-datum-hash $(cardano-cli transaction hash-script-data --script-data-value '"Hello PIGY!"') \
  --change-address $ADDRESS_1 \
  --tx-in-collateral 19c2a6bcedcf6ce8550c973537ae595ffdf4cea32b748cf0f08fa1667b3772bf#0 \
  --out-file tx.raw

cardano-cli transaction sign $MAGIC \
  --tx-body-file tx.raw \
  --out-file tx.signed \
  --signing-key-file keys/alonzo-purple.payment-1.skey

cardano-cli transaction submit $MAGIC \
  --tx-file tx.signed


cardano-cli query utxo $MAGIC --address $ADDRESS_S
cardano-cli query utxo $MAGIC --address $ADDRESS_1

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file alonzo-purple.protocol \
  --tx-in 0d904a7e8e635a96c6a70efe74da63020c1ad9d041132e0bc5b3f463ed16a5d5#1 \
    --tx-in-script-file $SCRIPT_FILE \
    --tx-in-datum-value '"Hello PIGY!"' \
    --tx-in-redeemer-value 2 \
  --tx-in aec4576aeaa7e394345d714c08a0205649bf3bec44544e19277d596a09b45a21#2 \
  --tx-in 0d904a7e8e635a96c6a70efe74da63020c1ad9d041132e0bc5b3f463ed16a5d5#0 \
  --tx-out "$ADDRESS_S+1800000+1 $CURRENCY.tSOFR" \
    --tx-out-datum-hash $(cardano-cli transaction hash-script-data --script-data-value '"Hello, PIGY?"') \
  --tx-out "$ADDRESS_1+5000000+10 $CURRENCY.tPIGY+1 $CURRENCY.tBRIO" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral 0d904a7e8e635a96c6a70efe74da63020c1ad9d041132e0bc5b3f463ed16a5d5#0 \
  --out-file tx.raw

cardano-cli transaction sign $MAGIC \
  --tx-body-file tx.raw \
  --out-file tx.signed \
  --signing-key-file keys/alonzo-purple.payment-1.skey

cardano-cli transaction submit $MAGIC \
  --tx-file tx.signed


cardano-cli query utxo $MAGIC --address $ADDRESS_S
cardano-cli query utxo $MAGIC --address $ADDRESS_1

