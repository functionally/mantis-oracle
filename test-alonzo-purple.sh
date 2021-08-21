# Set up the network.

MAGIC="--testnet-magic 8"

cardano-cli query protocol-parameters $MAGIC --out-file alonzo-purple.protocol


# Gather the test addresses.

ADDRESS_0=$(cat keys/alonzo-purple.payment-0.address); echo $ADDRESS_0

ADDRESS_1=$(cat keys/alonzo-purple.payment-1.address); echo $ADDRESS_1


# Create the minting policy.

CURRENCY=$(cardano-cli transaction policyid --script-file policy-0.script); echo $CURRENCY


# Create the script address.

SCRIPT_FILE=/scratch/code.functionally.io/sofr-oracle/repo/alonzo-purple.plutus; echo $SCRIPT_FILE

cabal run exe:mantis-oracle -- export $CURRENCY.tBRIO $CURRENCY.tSOFR $CURRENCY.tPIGY 10 $SCRIPT_FILE

ADDRESS_S=$(cardano-cli address build $MAGIC --payment-script-file $SCRIPT_FILE); echo $ADDRESS_S


# Create the initial datum.

DATUM='"Hello PIGY!"'; echo $DATUM
DATUM_HASH=$(cardano-cli transaction hash-script-data --script-data-value '"Hello PIGY!"'); echo $DATUM_HASH


# Collect test ADA.

curl -v -XPOST "https://faucet.alonzo-purple.dev.cardano.org/send-money/$ADDRESS_0?apiKey=jv3NBtZeaL0lZUxgqq8slTttX3BzViI7"

cardano-cli query utxo $MAGIC --address $ADDRESS_0

TXID_0=cbbdcdd3a1ef16002f771d2fbb5b83bedcee48a49dc2dd6e6065d60b307fb162


# Mint and distribute the tokens.

cardano-cli transaction build $MAGIC --alonzo-era \
  --tx-in $TXID_0#0 \
  --tx-out "$ADDRESS_1+100000000" \
  --tx-out "$ADDRESS_1+100000000+1000 $CURRENCY.tPIGY" \
  --tx-out "$ADDRESS_1+100000000+1 $CURRENCY.tSOFR" \
  --tx-out "$ADDRESS_1+100000000+1 $CURRENCY.tBRIO" \
  --change-address $ADDRESS_0 \
  --mint "1000 $CURRENCY.tPIGY+1 $CURRENCY.tSOFR+1 $CURRENCY.tBRIO" \
  --mint-script-file policy-0.script \
  --out-file tx.raw

cardano-cli transaction sign $MAGIC \
  --tx-body-file tx.raw \
  --out-file tx.signed \
  --signing-key-file keys/alonzo-purple.payment-0.skey

cardano-cli transaction submit $MAGIC \
  --tx-file tx.signed

cardano-cli query utxo $MAGIC --address $ADDRESS_0
cardano-cli query utxo $MAGIC --address $ADDRESS_1

TXID_1=b59680cb9a1f9bc2ff4501289308b9cc3a35d588f63f62cec71081c40fbec3bb


# Add the datum to the oracle.

cardano-cli transaction build $MAGIC --alonzo-era \
  --tx-in $TXID_1#3 \
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

TXID_2=408c15a001fc20158a9265c53b72041db1156b46ba640ded71d5c00365dc188d


# Read the oracle.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file alonzo-purple.protocol \
  --tx-in $TXID_2#1 \
    --tx-in-script-file $SCRIPT_FILE \
    --tx-in-datum-value '"Hello PIGY!"' \
    --tx-in-redeemer-value '1' \
  --tx-in $TXID_1#2 \
  --tx-out "$ADDRESS_S+5000000+1 $CURRENCY.tSOFR+10 $CURRENCY.tPIGY" \
    --tx-out-datum-hash $(cardano-cli transaction hash-script-data --script-data-value '"Hello PIGY!"') \
  --tx-out "$ADDRESS_1+5000000+990 $CURRENCY.tPIGY" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral $TXID_2#0 \
  --out-file tx.raw

cardano-cli transaction sign $MAGIC \
  --tx-body-file tx.raw \
  --out-file tx.signed \
  --signing-key-file keys/alonzo-purple.payment-1.skey

cardano-cli transaction submit $MAGIC \
  --tx-file tx.signed

cardano-cli query utxo $MAGIC --address $ADDRESS_S
cardano-cli query utxo $MAGIC --address $ADDRESS_1

TXID_3=b96fe94f7685ba889fd9f1473cfd40130ab34306fdbc1bf244d7282ec95d79b4


# Write the oracle.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file alonzo-purple.protocol \
  --tx-in $TXID_3#1 \
    --tx-in-script-file $SCRIPT_FILE \
    --tx-in-datum-value '"Hello PIGY!"' \
    --tx-in-redeemer-value 2 \
  --tx-in $TXID_1#4 \
  --tx-out "$ADDRESS_S+1800000+1 $CURRENCY.tSOFR" \
    --tx-out-datum-hash $(cardano-cli transaction hash-script-data --script-data-value '"Hello, PIGY?"') \
  --tx-out "$ADDRESS_1+5000000+10 $CURRENCY.tPIGY+1 $CURRENCY.tBRIO" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral $TXID_2#0 \
  --out-file tx.raw

cardano-cli transaction sign $MAGIC \
  --tx-body-file tx.raw \
  --out-file tx.signed \
  --signing-key-file keys/alonzo-purple.payment-1.skey

cardano-cli transaction submit $MAGIC \
  --tx-file tx.signed

cardano-cli query utxo $MAGIC --address $ADDRESS_S
cardano-cli query utxo $MAGIC --address $ADDRESS_1

TXID_4=4095ea6b029f07d2d476ef7c84c4a8c9920c49b402ed8a47c51dd8dda2bb9c3d


# Delete the oracle.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file alonzo-purple.protocol \
  --tx-in $TXID_4#1 \
    --tx-in-script-file $SCRIPT_FILE \
    --tx-in-datum-value '"Hello, PIGY?"' \
    --tx-in-redeemer-value 0 \
  --tx-in $TXID_4#2 \
  --tx-out "$ADDRESS_1+1500000+10 $CURRENCY.tPIGY" \
  --tx-out "$ADDRESS_1+1500000+1 $CURRENCY.tSOFR" \
  --tx-out "$ADDRESS_1+1500000+1 $CURRENCY.tBRIO" \
  --change-address $ADDRESS_1 \
  --tx-in-collateral $TXID_2#0 \
  --out-file tx.raw

cardano-cli transaction sign $MAGIC \
  --tx-body-file tx.raw \
  --out-file tx.signed \
  --signing-key-file keys/alonzo-purple.payment-1.skey

cardano-cli transaction submit $MAGIC \
  --tx-file tx.signed

cardano-cli query utxo $MAGIC --address $ADDRESS_S
cardano-cli query utxo $MAGIC --address $ADDRESS_1

TXID_5=aa4f3c682d3f7aee372811d4b7f49dc658db53ec7627df3b830dd0029404ad65


# Clean up the example so it can be run again.

cardano-cli transaction build $MAGIC --alonzo-era \
  --tx-in $TXID_1#0 \
  --tx-in $TXID_1#1 \
  --tx-in $TXID_2#0 \
  --tx-in $TXID_3#0 \
  --tx-in $TXID_3#2 \
  --tx-in $TXID_4#0 \
  --tx-in $TXID_5#0 \
  --tx-in $TXID_5#1 \
  --tx-in $TXID_5#2 \
  --tx-in $TXID_5#3 \
  --change-address $ADDRESS_0 \
  --mint "-1000 $CURRENCY.tPIGY+-1 $CURRENCY.tSOFR+-1 $CURRENCY.tBRIO" \
  --mint-script-file policy-0.script \
  --out-file tx.raw

cardano-cli transaction sign $MAGIC \
  --tx-body-file tx.raw \
  --out-file tx.signed \
  --signing-key-file keys/alonzo-purple.payment-0.skey \
  --signing-key-file keys/alonzo-purple.payment-1.skey

cardano-cli transaction submit $MAGIC \
  --tx-file tx.signed

cardano-cli query utxo $MAGIC --address $ADDRESS_S
cardano-cli query utxo $MAGIC --address $ADDRESS_0
cardano-cli query utxo $MAGIC --address $ADDRESS_1

TXID_0=80c81433724c7125694dd538f16b63e939f48438386721350c9e075bdfe0b44f
