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
  --tx-in 68a264b85baa2b844c3e38092c4150416c0cbac860f6c327cdd5125e9fe9564b#3 \
  --tx-in 4f5576b877dd5d65b3e8d9e368d51e22ba4208ea11b86b2a4d30a3a7c25b7293#0 \
  --tx-out "$ADDRESS_S+5000000+1 $CURRENCY.tSOFR" \
    --tx-out-datum-hash $DATUM_HASH \
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
  --tx-in 68a264b85baa2b844c3e38092c4150416c0cbac860f6c327cdd5125e9fe9564b#1 \
    --tx-in-script-file $SCRIPT_FILE \
    --tx-in-datum-value '"Hello PIGY!"' \
    --tx-in-redeemer-value 1 \
  --tx-in b5b7e0e9ebd27e50a32a0ba4fd783a557b93ab9dfe6b64f9b8e09cd8baebdd01#2 \
  --tx-in 68a264b85baa2b844c3e38092c4150416c0cbac860f6c327cdd5125e9fe9564b#0 \
  --tx-out "$ADDRESS_S+5000000+1 $CURRENCY.tSOFR+10 $CURRENCY.tPIGY" \
    --tx-out-datum-hash $(cardano-cli transaction hash-script-data --script-data-value '"Hello PIGY!"') \
  --tx-out "$ADDRESS_1+5000000+978 $CURRENCY.tPIGY" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral 68a264b85baa2b844c3e38092c4150416c0cbac860f6c327cdd5125e9fe9564b#0 \
  --out-file tx.raw

  --tx-out "$ADDRESS_S+6000000+10 $CURRENCY.tPIGY" \
    --tx-out-datum-hash $(cardano-cli transaction hash-script-data --script-data-value '"Hello PIGY!"') \

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
  --tx-in 92b8e3c9fa05136226c15c846db2ba00ae8a6f357ef466460733997016dae47f#1 \
    --tx-in-script-file $SCRIPT_FILE \
    --tx-in-datum-value '"Hello PIGY!"' \
    --tx-in-redeemer-value '-4' \
  --tx-in 4f5576b877dd5d65b3e8d9e368d51e22ba4208ea11b86b2a4d30a3a7c25b7293#2 \
  --tx-in 92b8e3c9fa05136226c15c846db2ba00ae8a6f357ef466460733997016dae47f#0 \
  --tx-out "$ADDRESS_S+1800000+1 $CURRENCY.tSOFR" \
    --tx-out-datum-hash $(cardano-cli transaction hash-script-data --script-data-value '"Hello, PIGY!"') \
  --tx-out "$ADDRESS_1+5000000+10 $CURRENCY.tPIGY+1 $CURRENCY.tBRIO" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral 92b8e3c9fa05136226c15c846db2ba00ae8a6f357ef466460733997016dae47f#0 \
  --out-file tx.raw

cardano-cli transaction sign $MAGIC \
  --tx-body-file tx.raw \
  --out-file tx.signed \
  --signing-key-file keys/alonzo-purple.payment-1.skey

cardano-cli transaction submit $MAGIC \
  --tx-file tx.signed


cardano-cli query utxo $MAGIC --address $ADDRESS_S
cardano-cli query utxo $MAGIC --address $ADDRESS_1

