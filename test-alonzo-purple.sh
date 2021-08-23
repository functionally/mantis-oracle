# Set up the network.

MAGIC="--testnet-magic 8"

cardano-cli query protocol-parameters $MAGIC --out-file alonzo-purple.protocol


# Gather the test addresses.

ADDRESS_0=$(cat keys/alonzo-purple.payment-0.address); echo $ADDRESS_0

ADDRESS_1=$(cat keys/alonzo-purple.payment-1.address); echo $ADDRESS_1


# Create the minting policy.

CURRENCY=$(cardano-cli transaction policyid --script-file policy-0.script); echo $CURRENCY


# Create the script address.

SCRIPT_FILE=alonzo-purple.plutus; echo $SCRIPT_FILE

cabal run exe:mantis-oracle -- export $CURRENCY.tBRIO $CURRENCY.tSOFR $CURRENCY.tPIGY 10 $SCRIPT_FILE

ADDRESS_S=$(cardano-cli address build $MAGIC --payment-script-file $SCRIPT_FILE); echo $ADDRESS_S


# Create the initial datum.

DATUM='"Hello PIGY!"'; echo $DATUM
DATUM_HASH=$(cardano-cli transaction hash-script-data --script-data-value '"Hello PIGY!"'); echo $DATUM_HASH


# Collect test ADA.

curl -v -XPOST "https://faucet.alonzo-purple.dev.cardano.org/send-money/$ADDRESS_0?apiKey=jv3NBtZeaL0lZUxgqq8slTttX3BzViI7"

cardano-cli query utxo $MAGIC --address $ADDRESS_0

TXID_0=a5ff9edf4e9b7140862ca3e15ff7c612bf7071f6a2b49f99f74d75698cbe4cb8


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

TXID_1=a649db30e00deb52a14aec2bf57272ed06698c2e842359fabb0145ceff0fe865


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

TXID_2=acf9efae154648ecf77d511b2a0847e0a5437c827a118aea44da5b15b23690a8


# Read the oracle.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file alonzo-purple.protocol \
  --tx-in $TXID_2#1 \
    --tx-in-script-file $SCRIPT_FILE \
    --tx-in-datum-value '"Hello PIGY!"' \
    --tx-in-redeemer-value '1' \
  --tx-in $TXID_1#2 \
  --tx-out "$ADDRESS_S+5000000+1 $CURRENCY.tSOFR+25 $CURRENCY.tPIGY" \
    --tx-out-datum-hash $(cardano-cli transaction hash-script-data --script-data-value '"Hello PIGY!"') \
  --tx-out "$ADDRESS_1+5000000+975 $CURRENCY.tPIGY" \
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

TXID_3=80561c48c1f84d8e78770d6343c49309653bdff11b9b243ab30f344a5b8f2882


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
  --tx-out "$ADDRESS_1+5000000+25 $CURRENCY.tPIGY+1 $CURRENCY.tBRIO" \
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

TXID_4=6ac2b23ad68f7faa0a3ba9be6f85274632db65f96294f76cece56ffd48ff49af


# Delete the oracle.

cardano-cli transaction build $MAGIC --alonzo-era \
  --protocol-params-file alonzo-purple.protocol \
  --tx-in $TXID_4#1 \
    --tx-in-script-file $SCRIPT_FILE \
    --tx-in-datum-value '"Hello, PIGY?"' \
    --tx-in-redeemer-value 0 \
  --tx-in $TXID_4#2 \
  --tx-out "$ADDRESS_1+1500000+25 $CURRENCY.tPIGY" \
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

TXID_5=162a93d00ed6a08a39a2d5558e4b8bd394c06aa02d392901cf0867d97dad6da8


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

TXID_0=b339a2748527326ebd08aa52417ff3c46c7436dd4eafb58043f43eec7041228a
