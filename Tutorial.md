Example using the command line
==============================

This example assumes the following:

*   Version 1.30.1 of `cardano-node` and the bundled version of `cardano-cli`.
*   `testnet.payment-0.address` contains the address of a wallet funded with test ADA.
*   `testnet.payment-0.skey` contains the corresponding signing key for the above address.
*   `testnet.payment-0.vkey` contains the corresponding verification key for the above address.
*   `testnet.payment-1.address` contains the address of a (possibly empty) address for use in the example.
*   `testnet.payment-1.skey` contains the corresponding signing key for the above address.
*   The control token for the oracle is named `tCORN`.
*   The datum token for the oracle is named `tFARM`.
*   The fee token for the oracle is named `tPIGY`.
*   Ten of the `tPIGY` tokens are needed to read the oracle, but no additional ADA is needed, aside from the transaction fee.
*   The `mantra-oracle` executable program is on the search path, given by the `PATH` environment variable.


Set up the network.
-------------------

    MAGIC="--testnet-magic 1097911063"
    
    cardano-cli query protocol-parameters $MAGIC --out-file testnet.protocol


Record the test addresses.
--------------------------

    ADDRESS_0=$(cat testnet.payment-0.address); echo $ADDRESS_0
    
    ADDRESS_1=$(cat testnet.payment-1.address); echo $ADDRESS_1


Create the minting policy.
--------------------------

    cat << EOI > testnet.policy-0.script
    {
        "type" : "sig"
    ,   "keyHash" : "$(cardano-cli address key-hash --payment-verification-key-file testnet.payment-0.vkey)"
    }
    EOI
    
    CURRENCY=$(cardano-cli transaction policyid --script-file testnet.policy-0.script); echo $CURRENCY


Configure the command-line tool.
--------------------------------

We use a configuration file like [testnet.mantra-oracle](testnet.mantra-oracle). Modify the path to the local node socket, and modify policy ID for the three assets to match `$CURRENCY`.

    Configuration
    {
      socketPath     = "/data/testnet.socket"
    , magic          = Just 1097911063
    , epochSlots     = 21600
    , controlAsset   = "13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tCORN"
    , datumAsset     = "13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tFARM"
    , feeAsset       = "13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY"
    , feeAmount      = 10
    , lovelaceAmount = 0
    }


Create the script address.
--------------------------

First, learn about the `export` command:

    $ mantra-oracle export --help
    
    Usage: mantra-oracle export CONFIG_FILE SCRIPT_FILE [CORE_FILE]
    
      Export the validator and compute its address.
    
    Available options:
      CONFIG_FILE              The configuration file.
      SCRIPT_FILE              Output filename for the serialized validator.
      CORE_FILE                Output filename for the Plutus Core code.
      -h,--help                Show this help text

Now export the Plutus script for the oracle and find its address:

    ORACLE_SCRIPT=testnet.plutus; echo $ORACLE_SCRIPT
    
    mantra-oracle export testnet.mantra-oracle $ORACLE_SCRIPT
    
    ADDRESS_S=$(cardano-cli address build $MAGIC --payment-script-file $ORACLE_SCRIPT); echo $ADDRESS_S


Find a UTxO with test ADA.
--------------------------

    $ cardano-cli query utxo $MAGIC --address $ADDRESS_0
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    a6cb48d8af22a773217199d4e4a75112f728b2e4744980a1861c96d088d01a90     0        193394880 lovelace + TxOutDatumHashNone

In this example, record that we have:

    TXID_0=a6cb48d8af22a773217199d4e4a75112f728b2e4744980a1861c96d088d01a90


Mint and distribute the tokens.
-------------------------------

    cardano-cli transaction build $MAGIC --alonzo-era \
      --tx-in $TXID_0#0 \
      --tx-out "$ADDRESS_1+30000000" \
      --tx-out "$ADDRESS_1+30000000+1000 $CURRENCY.tPIGY" \
      --tx-out "$ADDRESS_1+30000000+1 $CURRENCY.tCORN" \
      --tx-out "$ADDRESS_1+30000000+1 $CURRENCY.tFARM" \
      --tx-out "$ADDRESS_1+2000000" \
      --change-address $ADDRESS_0 \
      --mint "1000 $CURRENCY.tPIGY+1 $CURRENCY.tCORN+1 $CURRENCY.tFARM" \
      --mint-script-file testnet.policy-0.script \
      --out-file tx-1.raw

    cardano-cli transaction sign $MAGIC \
      --tx-body-file tx-1.raw \
      --out-file tx-1.signed \
      --signing-key-file testnet.payment-0.skey

    cardano-cli transaction submit $MAGIC \
      --tx-file tx-1.signed

Wait until the transaction is recorded on the blockchain and lookup the eUTxOs.

    $ cardano-cli query utxo $MAGIC --address $ADDRESS_1
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    7be8cd729a4324a7ae2650dbad202ee69ced5db2efd831090b94d2f2902d262a     1        30000000 lovelace + TxOutDatumHashNone
    7be8cd729a4324a7ae2650dbad202ee69ced5db2efd831090b94d2f2902d262a     2        30000000 lovelace + 1000 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHashNone
    7be8cd729a4324a7ae2650dbad202ee69ced5db2efd831090b94d2f2902d262a     3        30000000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tCORN + TxOutDatumHashNone
    7be8cd729a4324a7ae2650dbad202ee69ced5db2efd831090b94d2f2902d262a     4        30000000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tFARM + TxOutDatumHashNone
    7be8cd729a4324a7ae2650dbad202ee69ced5db2efd831090b94d2f2902d262a     5        2000000 lovelace + TxOutDatumHashNone0

In this example, record that we have:

    TXID_1=7be8cd729a4324a7ae2650dbad202ee69ced5db2efd831090b94d2f2902d262a


Create the oracle.
------------------

First, learn about the `create` command:

    $ mantra-oracle create --help
    
    Usage: mantra-oracle create CONFIG_FILE SIGNING_ADDRESS SIGNING_FILE
                                NEW_JSON_FILE [--collateral LOVELACE]
                                [--metadata INTEGER] [--message JSON_FILE]
                                [--lovelace LOVELACE]
      Create the oracle.
    
    Available options:
      CONFIG_FILE              The configuration file.
      SIGNING_ADDRESS          The address for the signing key.
      SIGNING_FILE             The signing key file.
      NEW_JSON_FILE            The JSON file for the new oracle data.
      --collateral LOVELACE    The maximum collateral for the transaction.
      --metadata INTEGER       The metadata key for the oracle data.
      --message JSON_FILE      The JSON file for the message metadata.
      --lovelace LOVELACE      The value to be sent to the script.
      -h,--help                Show this help text

We create the oracle with the example datum in [example-data-0.json](example-data-0.json). Note that the datum may be any JSON that can be serialized to CBOR.

    mantra-oracle create testnet.mantra-oracle \
                  $ADDRESS_1                   \
                  testnet.payment-1.skey       \
                  example-data-0.json

Wait until the transaction is recorded on the blockchain and look at the eUTxOs:

    $ cardano-cli query utxo $MAGIC --address $ADDRESS_S
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    bfbae31c35544639fb1177557282e17b3d06075161f1d1e2a2a72590f2de48a1     1        5000000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tFARM + TxOutDatumHash ScriptDataInAlonzoEra "4a11b3134a46a55dba9dad46a729984c6c4ef6e825c25ec540c586d273542788"
    
    $ cardano-cli query utxo $MAGIC --address $ADDRESS_1
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    7be8cd729a4324a7ae2650dbad202ee69ced5db2efd831090b94d2f2902d262a     2        30000000 lovelace + 1000 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHashNone
    bfbae31c35544639fb1177557282e17b3d06075161f1d1e2a2a72590f2de48a1     0        41298199 lovelace + TxOutDatumHashNone
    bfbae31c35544639fb1177557282e17b3d06075161f1d1e2a2a72590f2de48a1     2        43500000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tCORN + TxOutDatumHashNone
    bfbae31c35544639fb1177557282e17b3d06075161f1d1e2a2a72590f2de48a1     3        2000000 lovelace + TxOutDatumHashNone

In this example, record that we have:

    TXID_2=bfbae31c35544639fb1177557282e17b3d06075161f1d1e2a2a72590f2de48a1

The datum hash in the eUTxO should match the hash for the file:

    $ cardano-cli transaction hash-script-data --script-data-value "$(cat example-data-0.json)"
    
    4a11b3134a46a55dba9dad46a729984c6c4ef6e825c25ec540c586d273542788


Read the oracle.
----------------

Reading the oracle is typically done by another smart contract that needs to use the data in the oracle's eUTxO. For this example, we will create an example client smart contract and use `cardano-cli` to have it read the oracle. The source code is in [Mantra.Oracle.Reader](src/Mantra/Oracle/Reader.hs).

First, learn about the `reader` command:

    $ mantra-oracle reader --help
    
    Usage: mantra-oracle reader CONFIG_FILE OUTPUT_FILE
      Export an example validator for reading the oracle and compute its address.
    
    Available options:
      CONFIG_FILE              The configuration file.
      OUTPUT_FILE              Output filename for the serialized validator.
      -h,--help                Show this help text

Create the example Plutus script for reading the oracle.

    CLIENT_SCRIPT=example-reader.plutus; echo $CLIENT_SCRIPT
    
    mantra-oracle reader testnet.mantra-oracle $CLIENT_SCRIPT
    
    ADDRESS_C=$(cardano-cli address build $MAGIC --payment-script-file $CLIENT_SCRIPT); echo $ADDRESS_C

Now send some funds to that script.

    cardano-cli transaction build $MAGIC --alonzo-era \
      --protocol-params-file testnet.protocol \
      --tx-in $TXID_2#0 \
      --tx-out "$ADDRESS_C+1500000" \
        --tx-out-datum-hash $(cardano-cli transaction hash-script-data --script-data-value '"Hello"') \
      --change-address $ADDRESS_1 \
      --out-file tx-3.raw
    
    cardano-cli transaction sign $MAGIC \
      --tx-body-file tx-3.raw \
      --out-file tx-3.signed \
      --signing-key-file testnet.payment-1.skey
    
    cardano-cli transaction submit $MAGIC \
      --tx-file tx-3.signed

Wait until the transaction is recorded on the blockchain and look at the eUTxOs:

    $ cardano-cli query utxo $MAGIC --address $ADDRESS_C

                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    4f06438ff007e549cc1a522e348b7688595c94fe2b3f728db039712f709b5ebc     1        1500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "2cef1f3b66fb9aaf43dd90b2bd75e46dd55c942c46a3a9d035e8920e623e3208"

In this example, record that we have:

    TXID_3=4f06438ff007e549cc1a522e348b7688595c94fe2b3f728db039712f709b5ebc

The Plutus client script will only validate if it sees the correct datum in the oracle. Thus we submit a transaction that validates both the spending of the client and the reading of the oracle. The datum for the client script isn't used in its validation, and its redeemer must match the oracle's data: see `makeValidator` in [Mantra.Oracle.Reader](src/Mantra/Oracle/Reader.hs).

    cardano-cli transaction build $MAGIC --alonzo-era \
      --protocol-params-file testnet.protocol \
      --tx-in $TXID_3#1 \
        --tx-in-script-file $CLIENT_SCRIPT \
        --tx-in-datum-value '"Hello"' \
        --tx-in-redeemer-value '"PIGY!"' \
      --tx-in $TXID_2#1 \
        --tx-in-script-file $ORACLE_SCRIPT \
        --tx-in-datum-value "$(cat example-data-0.json)" \
        --tx-in-redeemer-file redeemer-read.json \
      --tx-in $TXID_1#2 \
      --tx-in $TXID_3#0 \
      --tx-out "$ADDRESS_S+5000000+1 $CURRENCY.tFARM+10 $CURRENCY.tPIGY" \
        --tx-out-datum-hash $(cardano-cli transaction hash-script-data --script-data-value "$(cat example-data-0.json)") \
      --tx-out "$ADDRESS_1+5000000+990 $CURRENCY.tPIGY" \
      --change-address $ADDRESS_1 \
      --tx-in-collateral $TXID_2#3 \
      --out-file tx-4.raw
    
    cardano-cli transaction sign $MAGIC \
      --tx-body-file tx-4.raw \
      --out-file tx-4.signed \
      --signing-key-file testnet.payment-1.skey
    
    cardano-cli transaction submit $MAGIC \
      --tx-file tx-4.signed

In the above, the file [redeemer-read.json](redeemer-read.json) contains the correct JSON serialization of the Oracle's `Read :: Action` redeemer.

Wait until the transaction is recorded on the blockchain and look at the eUTxOs:

    $ cardano-cli query utxo $MAGIC --address $ADDRESS_C
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    
    $ cardano-cli query utxo $MAGIC --address $ADDRESS_S
    
                              TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    06f5ba2d75b64699da7efbffbf5909e7e0e49d21d361d7f5c0dfb8bce99da06a     1        5000000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tFARM + 10 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHash ScriptDataInAlonzoEra "4a11b3134a46a55dba9dad46a729984c6c4ef6e825c25ec540c586d273542788"
        
    $ cardano-cli query utxo $MAGIC --address $ADDRESS_1
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    06f5ba2d75b64699da7efbffbf5909e7e0e49d21d361d7f5c0dfb8bce99da06a     0        65252290 lovelace + TxOutDatumHashNone
    06f5ba2d75b64699da7efbffbf5909e7e0e49d21d361d7f5c0dfb8bce99da06a     2        5000000 lovelace + 990 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHashNone
    bfbae31c35544639fb1177557282e17b3d06075161f1d1e2a2a72590f2de48a1     2        43500000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tCORN + TxOutDatumHashNone
    bfbae31c35544639fb1177557282e17b3d06075161f1d1e2a2a72590f2de48a1     3        2000000 lovelace + TxOutDatumHashNone

You can see that the oracle has acquired the `10 tPIGY` fee, but no additional ADA, and the client script spent its funds.

In this example, record that we have:

    TXID_4=06f5ba2d75b64699da7efbffbf5909e7e0e49d21d361d7f5c0dfb8bce99da06a


Write new data to the oracle.
-----------------------------

First, learn about the `write` command:

    $ mantra-oracle write --help
    
    Usage: mantra-oracle write CONFIG_FILE SIGNING_ADDRESS SIGNING_FILE
                               OLD_JSON_FILE NEW_JSON_FILE [--collateral LOVELACE]
                               [--metadata INTEGER] [--message JSON_FILE]
                               [--lovelace LOVELACE]
      Write a value to the oracle.
    
    Available options:
      CONFIG_FILE              The configuration file.
      SIGNING_ADDRESS          The address for the signing key.
      SIGNING_FILE             The signing key file.
      OLD_JSON_FILE            The JSON file for the existing oracle data.
      NEW_JSON_FILE            The JSON file for the new oracle data.
      --collateral LOVELACE    The maximum collateral for the transaction.
      --metadata INTEGER       The metadata key for the oracle data.
      --message JSON_FILE      The JSON file for the message metadata.
      --lovelace LOVELACE      The value to be sent to the script.
      -h,--help                Show this help text

We replace the oracle's datum from [example-data-0.json](example-data-0.json) with new data in [example-data-1.json](example-data-1.json).

    mantra-oracle write testnet.mantra-oracle \
                  $ADDRESS_1                  \
                  testnet.payment-1.skey      \
                  example-data-0.json         \
                  example-data-1.json

Wait until the transaction is recorded on the blockchain and look at the eUTxOs:

    $ cardano-cli query utxo $MAGIC --address $ADDRESS_S
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    c704a3d0697eae1676e1eeb020ae201c5145e5cbd835032d9eaffb66f41008af     1        5000000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tFARM + TxOutDatumHash ScriptDataInAlonzoEra "b9c12181970235c54ec71f27310911e5a70b49995d8f5049c500e2846676091c"
    
    $ cardano-cli query utxo $MAGIC --address $ADDRESS_1
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    06f5ba2d75b64699da7efbffbf5909e7e0e49d21d361d7f5c0dfb8bce99da06a     2        5000000 lovelace + 990 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHashNone
    c704a3d0697eae1676e1eeb020ae201c5145e5cbd835032d9eaffb66f41008af     0        52731327 lovelace + TxOutDatumHashNone
    c704a3d0697eae1676e1eeb020ae201c5145e5cbd835032d9eaffb66f41008af     2        55376145 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tCORN + 10 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHashNone
    c704a3d0697eae1676e1eeb020ae201c5145e5cbd835032d9eaffb66f41008af     3        2000000 lovelace + TxOutDatumHashNone

In this example, record that we have:

    TXID_5=c704a3d0697eae1676e1eeb020ae201c5145e5cbd835032d9eaffb66f41008af

The datum hash in the eUTxO will match the hash for the new file:

    $ cardano-cli transaction hash-script-data --script-data-value "$(cat example-data-1.json)"
    
    b9c12181970235c54ec71f27310911e5a70b49995d8f5049c500e2846676091c


Delete the oracle.
------------------

First, learn about the `delete` command:

    $ mantra-oracle delete --help
    
    Usage: mantra-oracle delete CONFIG_FILE SIGNING_ADDRESS SIGNING_FILE
                                OLD_JSON_FILE [--collateral LOVELACE]
                                [--message JSON_FILE] [--lovelace LOVELACE]
      Delete the oracle.
    
    Available options:
      CONFIG_FILE              The configuration file.
      SIGNING_ADDRESS          The address for the signing key.
      SIGNING_FILE             The signing key file.
      OLD_JSON_FILE            The JSON file for the existing oracle data.
      --collateral LOVELACE    The maximum collateral for the transaction.
      --message JSON_FILE      The JSON file for the message metadata.
      --lovelace LOVELACE      The value to be sent to the script.
      -h,--help                Show this help text

We delete the oracle with the example datum in [example-data-1.json](example-data-1.json).

    mantra-oracle delete testnet.mantra-oracle \
                  $ADDRESS_1                   \
                  testnet.payment-1.skey       \
                  example-data-1.json

Wait until the transaction is recorded on the blockchain and look at the eUTxOs:

    $ cardano-cli query utxo $MAGIC --address $ADDRESS_S
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    
    $ cardano-cli query utxo $MAGIC --address $ADDRESS_1
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    06f5ba2d75b64699da7efbffbf5909e7e0e49d21d361d7f5c0dfb8bce99da06a     2        5000000 lovelace + 990 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHashNone
    a00e0586be7a179cd7ffb72c9b6a01e4691f77db2191f8d5ebdfb93a17a14405     0        52423290 lovelace + TxOutDatumHashNone
    a00e0586be7a179cd7ffb72c9b6a01e4691f77db2191f8d5ebdfb93a17a14405     1        5000000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tFARM + TxOutDatumHashNone
    a00e0586be7a179cd7ffb72c9b6a01e4691f77db2191f8d5ebdfb93a17a14405     2        55053736 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tCORN + 10 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHashNone
    a00e0586be7a179cd7ffb72c9b6a01e4691f77db2191f8d5ebdfb93a17a14405     3        2000000 lovelace + TxOutDatumHashNone

There should be no eUTxOs at the script address.

In this example, record that we have:

    TXID_6=a00e0586be7a179cd7ffb72c9b6a01e4691f77db2191f8d5ebdfb93a17a14405


Clean up the example so it can be run again.
--------------------------------------------

We can burn the test tokens and collect the remaining test ADA from this example.

    cardano-cli transaction build $MAGIC --alonzo-era \
      --tx-in $TXID_1#0 \
      --tx-in $TXID_4#2 \
      --tx-in $TXID_6#0 \
      --tx-in $TXID_6#1 \
      --tx-in $TXID_6#2 \
      --tx-in $TXID_6#3 \
      --change-address $ADDRESS_0 \
      --mint "-1000 $CURRENCY.tPIGY+-1 $CURRENCY.tFARM+-1 $CURRENCY.tCORN" \
      --mint-script-file testnet.policy-0.script \
      --out-file tx-0.raw
    
    cardano-cli transaction sign $MAGIC \
      --tx-body-file tx-0.raw \
      --out-file tx-0.signed \
      --signing-key-file testnet.payment-0.skey \
      --signing-key-file testnet.payment-1.skey
    
    cardano-cli transaction submit $MAGIC \
      --tx-file tx-0.signed

Wait until the transaction is recorded on the blockchain and look at the eUTxOs:

    $ cardano-cli query utxo $MAGIC --address $ADDRESS_S
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    
    $ cardano-cli query utxo $MAGIC --address $ADDRESS_0
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    c3bfe685f600758c1093a942dda9d8312ed1191a3e0d5148015d0bb8ff416f73     0        190483528 lovelace + TxOutDatumHashNone

    $ cardano-cli query utxo $MAGIC --address $ADDRESS_1
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------

In this example, record that we have:

    TXID_0=c3bfe685f600758c1093a942dda9d8312ed1191a3e0d5148015d0bb8ff416f73
