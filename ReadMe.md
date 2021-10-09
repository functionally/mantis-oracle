The Mantra Oracle: A General-Purpose Token-Based Oracle for Cardano
===================================================================

This Cardano oracle reports structured data (namely, the `PlutuxTx.BuiltinData` type) to a transaction if the fee, as a quantity of a fungible token, is paid. It can be incorporated into other smart-contract scripts that use the oracle's value in their validation logic.

An example of this oracle in action on `testnet` is described at [https://github.com/pigytoken/pigy-delegation/blob/main/oracle/](https://github.com/pigytoken/pigy-delegation/blob/main/oracle/ReadMe.md).


Parameters
----------

The oracle is parameterized as follows:

    data Parameters =
      Parameters
      {
        controlParameter :: AssetClass
      , datumParameter   :: AssetClass
      , feeToken         :: AssetClass
      , feeAmount        :: Integer
      , lovelaceAmount   :: Integer
      }

*   The `controlParameter` specifies a native token the that controller (owner) of the oracle uses to update the oracle's value, withdraw funds, or close (delete) the oracle.
*   The `datumParameter` specifies the non-fungible token that identifies the UTxO containing the oracle data.
*   The quantity `feeAmount` of the fungible token `feeToken` must be paid to the oracle in order to read its value in an on-chain validator.
*   The quantity `lovelaceAmount` of ADA must be paid to the oracle in order to read its value in an on-chain validator.


Actions
-------

The oracle can perform three simple actions:

    data Action =
        Delete
      | Read
      | Write

*   The `Read` action (`redeemer = 1`) simply reads the value of the oracle into a transaction, and corresponds to the `read` endpoint in [`Mantra.Oracle.Client`](src/Mantra/Oracle/Client.hs). The required fee must be paid to the oracle script, and the UTxO containing the oracle data must be consumed and paid back to the oracle script, with the data unchanged.
*   The `Write` action (`redeemer = 2`) updates the value of the oracle, and corresponds to the `write` endpoint in [`Mantra.Oracle.Controller`](src/Mantra/Oracle/Controller.hs). The control token (specified by `controlParameter`) must be present in the transaction, and the UTxO containing the oracle data must be consumed and paid back (with the revised data) to the oracle script.
*   The `Delete` action (`redeemer = 0`) shuts down the oracle by removing the data and associated NFT, and corresponds to the `delete` endpoint in [`Mantra.Oracle.Controller`](src/Mantra/Oracle/Controller.hs). The control token must be present in the transaction.


Usage
-----

The oracle can be incorporated into other smart-contract scripts that use the oracle's value in their validation logic via the `Read` redeemer. Here is an example use case of creating, reading, and writing to the oracle.

![Example transactions using the oracle.](transactions.png)


Installation
------------

This package uses the [`haskell.nix`](https://input-output-hk.github.io/haskell.nix/) build system. Simply clone this repository and execute the build command:

    nix-build -A mantra-oracle.components.exes.mantra-oracle -o build

The executable result will be in `./build/bin/mantra-oracle`.

Alternatively, one can use the `cabal install` installation approach, which relies on the [cabal.project](cabal.project) file and which is known to succeed with cabal 3.4.0.0 and ghc 8.10.4.


Example using the command line
------------------------------

This example assumes the following:

*   Version 1.29.0 of `cardano-node` and the bundled version of `cardano-cli`.
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


### Set up the network.

    MAGIC="--testnet-magic 1097911063"
    
    cardano-cli query protocol-parameters $MAGIC --out-file testnet.protocol


### Record the test addresses.

    ADDRESS_0=$(cat testnet.payment-0.address); echo $ADDRESS_0
    
    ADDRESS_1=$(cat testnet.payment-1.address); echo $ADDRESS_1


### Create the minting policy.

    cat << EOI > testnet.policy-0.script
    {
        "type" : "sig"
    ,   "keyHash" : "$(cardano-cli address key-hash --payment-verification-key-file testnet.payment-0.vkey)"
    }
    EOI
    
    CURRENCY=$(cardano-cli transaction policyid --script-file testnet.policy-0.script); echo $CURRENCY


### Configure the command-line tool.

We use a configuration file like [testnet.mantra-oracle](testnet.mantra-oracle). Modify the path to the local node socket, and modify policy ID for the three assets to match `$CURRENCY`.

    Configuration
    {
      socketPath     = "/data/testnet.socket"
    , magic          = Just 1097911063
    , epochSlots     = 43200
    , controlAsset   = "13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tBRIO"
    , datumAsset     = "13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tSOFR"
    , feeAsset       = "13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY"
    , feeAmount      = 10
    , lovelaceAmount = 0
    }


### Create the script address.

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


### Find a UTxO with test ADA.

    $ cardano-cli query utxo $MAGIC --address $ADDRESS_0
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    7d6fd5ad29537eb0af7ab30a33203d4af5131c0cd85bbe2547f74d24c4ca273a     0        196700690 lovelace + TxOutDatumHashNone

In this example, record that we have:

    TXID_0=7d6fd5ad29537eb0af7ab30a33203d4af5131c0cd85bbe2547f74d24c4ca273a


### Mint and distribute the tokens.

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
      --out-file tx.raw
    
    cardano-cli transaction sign $MAGIC \
      --tx-body-file tx-1.raw \
      --out-file tx.signed \
      --signing-key-file testnet.payment-0.skey
    
    cardano-cli transaction submit $MAGIC \
      --tx-file tx.signed

Wait until the transaction is recorded on the blockchain and lookup the eUTxOs.

    $ cardano-cli query utxo $MAGIC --address $ADDRESS_1
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    c74b19eeefb261728feb44e038d460d266fc1896de12d26413eb2affa17b3ead     1        30000000 lovelace + TxOutDatumHashNone
    c74b19eeefb261728feb44e038d460d266fc1896de12d26413eb2affa17b3ead     2        30000000 lovelace + 1000 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHashNone
    c74b19eeefb261728feb44e038d460d266fc1896de12d26413eb2affa17b3ead     3        30000000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tCORN + TxOutDatumHashNone
    c74b19eeefb261728feb44e038d460d266fc1896de12d26413eb2affa17b3ead     4        30000000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tFARM + TxOutDatumHashNone
    c74b19eeefb261728feb44e038d460d266fc1896de12d26413eb2affa17b3ead     5        2000000 lovelace + TxOutDatumHashNone

In this example, record that we have:

    TXID_1=c74b19eeefb261728feb44e038d460d266fc1896de12d26413eb2affa17b3ead


### Create the oracle.

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
    dff1a77dde2937a305fd2fd967436754171cbcdecbdb07f1b57e492ab3f50bb2     1        5000000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tFARM + TxOutDatumHash ScriptDataInAlonzoEra "4a11b3134a46a55dba9dad46a729984c6c4ef6e825c25ec540c586d273542788"
    
    $ cardano-cli query utxo $MAGIC --address $ADDRESS_1
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    c74b19eeefb261728feb44e038d460d266fc1896de12d26413eb2affa17b3ead     2        30000000 lovelace + 1000 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHashNone
    dff1a77dde2937a305fd2fd967436754171cbcdecbdb07f1b57e492ab3f50bb2     0        41298199 lovelace + TxOutDatumHashNone
    dff1a77dde2937a305fd2fd967436754171cbcdecbdb07f1b57e492ab3f50bb2     2        43500000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tCORN + TxOutDatumHashNone
    dff1a77dde2937a305fd2fd967436754171cbcdecbdb07f1b57e492ab3f50bb2     3        2000000 lovelace + TxOutDatumHashNone

In this example, record that we have:

    TXID_2=dff1a77dde2937a305fd2fd967436754171cbcdecbdb07f1b57e492ab3f50bb2

The datum hash in the eUTxO should match the hash for the file:

    $ cardano-cli transaction hash-script-data --script-data-value "$(cat example-data-0.json)"
    
    4a11b3134a46a55dba9dad46a729984c6c4ef6e825c25ec540c586d273542788


### Read the oracle.

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
      --out-file tx.raw
    
    cardano-cli transaction sign $MAGIC \
      --tx-body-file tx.raw \
      --out-file tx.signed \
      --signing-key-file testnet.payment-1.skey
    
    cardano-cli transaction submit $MAGIC \
      --tx-file tx.signed

Wait until the transaction is recorded on the blockchain and look at the eUTxOs:

    $ cardano-cli query utxo $MAGIC --address $ADDRESS_C

                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    da347c86803994e1166999a800bbe6963c9d016588dce05ce6fca2ad54596853     1        1500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "2cef1f3b66fb9aaf43dd90b2bd75e46dd55c942c46a3a9d035e8920e623e3208"

In this example, record that we have:

    TXID_3=da347c86803994e1166999a800bbe6963c9d016588dce05ce6fca2ad54596853

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
        --tx-in-redeemer-value '1' \
      --tx-in $TXID_1#2 \
      --tx-in $TXID_3#0 \
      --tx-out "$ADDRESS_S+5000000+1 $CURRENCY.tFARM+10 $CURRENCY.tPIGY" \
        --tx-out-datum-hash $(cardano-cli transaction hash-script-data --script-data-value "$(cat example-data-0.json)") \
      --tx-out "$ADDRESS_1+5000000+990 $CURRENCY.tPIGY" \
      --change-address $ADDRESS_1 \
      --tx-in-collateral $TXID_2#3 \
      --out-file tx.raw
    
    cardano-cli transaction sign $MAGIC \
      --tx-body-file tx.raw \
      --out-file tx.signed \
      --signing-key-file testnet.payment-1.skey
    
    cardano-cli transaction submit $MAGIC \
      --tx-file tx.signed

Wait until the transaction is recorded on the blockchain and look at the eUTxOs:

    $ cardano-cli query utxo $MAGIC --address $ADDRESS_C
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    
    $ cardano-cli query utxo $MAGIC --address $ADDRESS_S
    
                              TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    f740a4c0365f88e345d85a7bf57822e3768327d1cb0e486ebcbac008782db07f     1        5000000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tFARM + 10 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHash ScriptDataInAlonzoEra "4a11b3134a46a55dba9dad46a729984c6c4ef6e825c25ec540c586d273542788"
        
    $ cardano-cli query utxo $MAGIC --address $ADDRESS_1
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    dff1a77dde2937a305fd2fd967436754171cbcdecbdb07f1b57e492ab3f50bb2     2        43500000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tCORN + TxOutDatumHashNone
    dff1a77dde2937a305fd2fd967436754171cbcdecbdb07f1b57e492ab3f50bb2     3        2000000 lovelace + TxOutDatumHashNone
    f740a4c0365f88e345d85a7bf57822e3768327d1cb0e486ebcbac008782db07f     0        65069553 lovelace + TxOutDatumHashNone
    f740a4c0365f88e345d85a7bf57822e3768327d1cb0e486ebcbac008782db07f     2        5000000 lovelace + 990 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHashNone

You can see that the oracle has acquired the `10 tPIGY` fee, but no additional ADA, and the client script spent its funds.

In this example, record that we have:

    TXID_4=f740a4c0365f88e345d85a7bf57822e3768327d1cb0e486ebcbac008782db07f


### Write new data to the oracle.

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
    c99ba60c8b58e16fec6e103d7c7c08c6071d311f1755d5778cee0c0c4beeb9fa     1        5000000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tFARM + TxOutDatumHash ScriptDataInAlonzoEra "b9c12181970235c54ec71f27310911e5a70b49995d8f5049c500e2846676091c"
    
    $ cardano-cli query utxo $MAGIC --address $ADDRESS_1
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    c99ba60c8b58e16fec6e103d7c7c08c6071d311f1755d5778cee0c0c4beeb9fa     0        52529888 lovelace + TxOutDatumHashNone
    c99ba60c8b58e16fec6e103d7c7c08c6071d311f1755d5778cee0c0c4beeb9fa     2        55284777 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tCORN + 10 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHashNone
    c99ba60c8b58e16fec6e103d7c7c08c6071d311f1755d5778cee0c0c4beeb9fa     3        2000000 lovelace + TxOutDatumHashNone
    f740a4c0365f88e345d85a7bf57822e3768327d1cb0e486ebcbac008782db07f     2        5000000 lovelace + 990 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHashNone

In this example, record that we have:

    TXID_5=c99ba60c8b58e16fec6e103d7c7c08c6071d311f1755d5778cee0c0c4beeb9fa

The datum hash in the eUTxO will match the hash for the new file:

    $ cardano-cli transaction hash-script-data --script-data-value "$(cat example-data-1.json)"
    
    b9c12181970235c54ec71f27310911e5a70b49995d8f5049c500e2846676091c


### Delete the oracle.

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
    015ba433c08774993aaf5ff3682028689b1a0de33b55dedad565d7e4383dc9b2     0        52175235 lovelace + TxOutDatumHashNone
    015ba433c08774993aaf5ff3682028689b1a0de33b55dedad565d7e4383dc9b2     1        5000000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tFARM + TxOutDatumHashNone
    015ba433c08774993aaf5ff3682028689b1a0de33b55dedad565d7e4383dc9b2     2        54907333 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tCORN + 10 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHashNone
    015ba433c08774993aaf5ff3682028689b1a0de33b55dedad565d7e4383dc9b2     3        2000000 lovelace + TxOutDatumHashNone
    f740a4c0365f88e345d85a7bf57822e3768327d1cb0e486ebcbac008782db07f     2        5000000 lovelace + 990 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHashNone

There should be no eUTxOs at the script address.

In this example, record that we have:

    TXID_6=015ba433c08774993aaf5ff3682028689b1a0de33b55dedad565d7e4383dc9b2


### Clean up the example so it can be run again.

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
      --out-file tx.raw
    
    cardano-cli transaction sign $MAGIC \
      --tx-body-file tx.raw \
      --out-file tx.signed \
      --signing-key-file testnet.payment-0.skey \
      --signing-key-file testnet.payment-1.skey
    
    cardano-cli transaction submit $MAGIC \
      --tx-file tx.signed

Wait until the transaction is recorded on the blockchain and look at the eUTxOs:

    $ cardano-cli query utxo $MAGIC --address $ADDRESS_S
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    
    $ cardano-cli query utxo $MAGIC --address $ADDRESS_0
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    a6cb48d8af22a773217199d4e4a75112f728b2e4744980a1861c96d088d01a90     0        193394880 lovelace + TxOutDatumHashNone

    $ cardano-cli query utxo $MAGIC --address $ADDRESS_1
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------

In this example, record that we have:

    TXID_0=a6cb48d8af22a773217199d4e4a75112f728b2e4744980a1861c96d088d01a90


Simulation and PAB Examples
---------------------------

The oracle can be incorporated into other smart-contract scripts that use the oracle's value in their validation logic via the `readOracleConstraints` function in [`Mantra.Oracle.Client`](src/Mantra/Oracle/Client.hs), which returns the correct lookups, transaction constraints, and datum for a script endpoint to employ the oracle. The `readOracle` function is the simplest example of an endpoint: it just reads the oracle value and performs no other actions.

See the slighly older version of this tool at https://github.com/functionally/mantra-oracle/blob/51d21574dd2a11280ece72068d56ef33f5672404/ReadMe.md for examples use the Plutus simulator and the Plutus Application Backend. Full simulator and PAB support will be included when the PAB is released.


Testing
-------

The [test suite](tests/ReadMe.md) contains 29 tests that provide complete coverage for the logical and redemption of the Plutus validator for the oracle.


API documentation
-----------------

See https://functionally.github.io/mantra-oracle/ for API documentation.


Development environment
-----------------------

Due to quirks in how [`haskell.nix`](https://input-output-hk.github.io/haskell.nix/) and [`cabal.project`](https://cabal.readthedocs.io/en/3.4/cabal-project.html) interact, the following procedure needs to be followed to create a development environment for compiling `mantra`:

1.  Run `nix-shell`. This takes a while to build unless you set `withHoogle = false` in [shell.nix](shell.nix).
2.  Temporarily comment-out the `source-repository-package` lines in [cabal.project](cabal.project).
3.  Run `cabal build`, `hoogle`, or other development tools defined in [shell.nix](shell.nix).
