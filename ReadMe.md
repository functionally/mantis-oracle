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

*   The `Read` action simply reads the value of the oracle into a transaction, and corresponds to the `read` endpoint in [`Mantra.Oracle.Client`](src/Mantra/Oracle/Client.hs). The required fee must be paid to the oracle script, and the UTxO containing the oracle data must be consumed and paid back to the oracle script, with the data unchanged.
*   The `Write` action updates the value of the oracle, and corresponds to the `write` endpoint in [`Mantra.Oracle.Controller`](src/Mantra/Oracle/Controller.hs). The control token (specified by `controlParameter`) must be present in the transaction, and the UTxO containing the oracle data must be consumed and paid back (with the revised data) to the oracle script.
*   The `Delete` action shuts down the oracle by removing the data and associated NFT, and corresponds to the `delete` endpoint in [`Mantra.Oracle.Controller`](src/Mantra/Oracle/Controller.hs). The control token must be present in the transaction.


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
    
    Usage: mantra-oracle export CONFIG_FILE OUTPUT_FILE
    
      Export the validator and compute its address.
    
    Available options:
      CONFIG_FILE              The configuration file.
      OUTPUT_FILE              Output filename for the serialized validator.
      -h,--help                Show this help text

Now export the Plutus script for the oracle and find its address:

    ORACLE_SCRIPT=testnet.plutus; echo $ORACLE_SCRIPT
    
    mantra-oracle export testnet.mantra-oracle $ORACLE_SCRIPT
    
    ADDRESS_S=$(cardano-cli address build $MAGIC --payment-script-file $ORACLE_SCRIPT); echo $ADDRESS_S


### Find a UTxO with test ADA.

    $ cardano-cli query utxo $MAGIC --address $ADDRESS_0
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    cb1428e6277e3133ece6266af85151bb9d36d942486386e3e3abb0f0b3cffdca     0        597811135 lovelace + TxOutDatumHashNone

In this example, record that we have:

    TXID_0=cb1428e6277e3133ece6266af85151bb9d36d942486386e3e3abb0f0b3cffdca


### Mint and distribute the tokens.

    cardano-cli transaction build $MAGIC --alonzo-era \
      --tx-in $TXID_0#0 \
      --tx-out "$ADDRESS_1+100000000" \
      --tx-out "$ADDRESS_1+100000000+1000 $CURRENCY.tPIGY" \
      --tx-out "$ADDRESS_1+100000000+1 $CURRENCY.tCORN" \
      --tx-out "$ADDRESS_1+100000000+1 $CURRENCY.tFARM" \
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
    00fdf860bb1254ac1e9a2641b5de93cf9c3e642cf7485d88253cf08f1b672126     1        100000000 lovelace + TxOutDatumHashNone
    00fdf860bb1254ac1e9a2641b5de93cf9c3e642cf7485d88253cf08f1b672126     2        100000000 lovelace + 1000 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHashNone
    00fdf860bb1254ac1e9a2641b5de93cf9c3e642cf7485d88253cf08f1b672126     3        100000000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tCORN + TxOutDatumHashNone
    00fdf860bb1254ac1e9a2641b5de93cf9c3e642cf7485d88253cf08f1b672126     4        100000000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tFARM + TxOutDatumHashNone
    00fdf860bb1254ac1e9a2641b5de93cf9c3e642cf7485d88253cf08f1b672126     5        2000000 lovelace + TxOutDatumHashNone

In this example, record that we have:

    TXID_1=00fdf860bb1254ac1e9a2641b5de93cf9c3e642cf7485d88253cf08f1b672126


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
    a04473dfc0e7eb9bdd9e509c26b3d76ef1edde9b22b96003577261b8befc80bb     1        5000000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tFARM + TxOutDatumHash ScriptDataInAlonzoEra "27f7dc5d54a5a5cfa25f75f200f62f5a3decce6ca05f4497ce3123d8b3f682ad"
    
    $ cardano-cli query utxo $MAGIC --address $ADDRESS_1
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    00fdf860bb1254ac1e9a2641b5de93cf9c3e642cf7485d88253cf08f1b672126     2        100000000 lovelace + 1000 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHashNone
    a04473dfc0e7eb9bdd9e509c26b3d76ef1edde9b22b96003577261b8befc80bb     0        146298199 lovelace + TxOutDatumHashNone
    a04473dfc0e7eb9bdd9e509c26b3d76ef1edde9b22b96003577261b8befc80bb     2        148500000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tCORN + TxOutDatumHashNone
    a04473dfc0e7eb9bdd9e509c26b3d76ef1edde9b22b96003577261b8befc80bb     3        2000000 lovelace + TxOutDatumHashNone

In this example, record that we have:

    TXID_2=a04473dfc0e7eb9bdd9e509c26b3d76ef1edde9b22b96003577261b8befc80bb

The datum hash in the eUTxO should match the hash for the file:

    $ cardano-cli transaction hash-script-data --script-data-value "$(cat example-data-0.json)"
    
    27f7dc5d54a5a5cfa25f75f200f62f5a3decce6ca05f4497ce3123d8b3f682ad


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
        --tx-out-datum-hash $(cardano-cli transaction hash-script-data --script-data-value '"irrelevant"') \
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
    4e2b3dc2a57c2151c555faae9131eb43cdf099375c5236a3c758be3aa0501082     1        1500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "f2b0cdd38c1661d489fa162aa222d8744a84fb411e3393cc3041913f1c4d4822"

In this example, record that we have:

    TXID_3=4e2b3dc2a57c2151c555faae9131eb43cdf099375c5236a3c758be3aa0501082

The Plutus client script will only validate if it sees the correct datum in the oracle. Thus we submit a transaction that validates both the spending of the client and the reading of the oracle. The datum for the client script isn't used in its validation, and its redeemer must match the oracle's data: see `makeValidator` in [Mantra.Oracle.Reader](src/Mantra/Oracle/Reader.hs).

    cardano-cli transaction build $MAGIC --alonzo-era \
      --protocol-params-file testnet.protocol \
      --tx-in $TXID_3#1 \
        --tx-in-script-file $CLIENT_SCRIPT \
        --tx-in-datum-value '"irrelevant"' \
        --tx-in-redeemer-value '"Hello PIGY!"' \
      --tx-in $TXID_2#1 \
        --tx-in-script-file $ORACLE_SCRIPT \
        --tx-in-datum-value "$(cat example-data-0.json)" \
        --tx-in-redeemer-value '1' \
      --tx-in $TXID_1#2 \
      --tx-in $TXID_3#0 \
      --tx-out "$ADDRESS_S+5000000+1 $CURRENCY.tFARM+10 $CURRENCY.tPIGY" \
        --tx-out-datum-hash $(cardano-cli transaction hash-script-data --script-data-value '"Hello PIGY!"') \
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
    2c7fe4a5e0a142cf38092d1b9873a9a2a2774f0833771832316eca16ffcd95a9     1        5000000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tFARM + 10 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHash ScriptDataInAlonzoEra "27f7dc5d54a5a5cfa25f75f200f62f5a3decce6ca05f4497ce3123d8b3f682ad"
        
    $ cardano-cli query utxo $MAGIC --address $ADDRESS_1
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    2c7fe4a5e0a142cf38092d1b9873a9a2a2774f0833771832316eca16ffcd95a9     0        240085838 lovelace + TxOutDatumHashNone
    2c7fe4a5e0a142cf38092d1b9873a9a2a2774f0833771832316eca16ffcd95a9     2        5000000 lovelace + 990 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHashNone
    a04473dfc0e7eb9bdd9e509c26b3d76ef1edde9b22b96003577261b8befc80bb     2        148500000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tCORN + TxOutDatumHashNone
    a04473dfc0e7eb9bdd9e509c26b3d76ef1edde9b22b96003577261b8befc80bb     3        2000000 lovelace + TxOutDatumHashNone

You can see that the oracle has acquired the `10 tPIGY` fee, but no additional ADA, and the client script spent its funds.

In this example, record that we have:

    TXID_4=2c7fe4a5e0a142cf38092d1b9873a9a2a2774f0833771832316eca16ffcd95a9


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
    24607ec7c04e1376fe637fbc649fe65bffcc7cb57bc4468509f38988c9556d34     1        5000000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tFARM + TxOutDatumHash ScriptDataInAlonzoEra "000448b2cf8aeea0eac22d1e7a5cb6dd286e3473be70de4c6e25e9342f1b3dff"
    
    $ cardano-cli query utxo $MAGIC --address $ADDRESS_1
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    24607ec7c04e1376fe637fbc649fe65bffcc7cb57bc4468509f38988c9556d34     0        192543936 lovelace + TxOutDatumHashNone
    24607ec7c04e1376fe637fbc649fe65bffcc7cb57bc4468509f38988c9556d34     2        195292919 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tCORN + 10 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHashNone
    24607ec7c04e1376fe637fbc649fe65bffcc7cb57bc4468509f38988c9556d34     3        2000000 lovelace + TxOutDatumHashNone
    2c7fe4a5e0a142cf38092d1b9873a9a2a2774f0833771832316eca16ffcd95a9     2        5000000 lovelace + 990 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHashNone

In this example, record that we have:

    TXID_5=24607ec7c04e1376fe637fbc649fe65bffcc7cb57bc4468509f38988c9556d34

The datum hash in the eUTxO will match the hash for the new file:

    $ cardano-cli transaction hash-script-data --script-data-value "$(cat example-data-1.json)"
    
    000448b2cf8aeea0eac22d1e7a5cb6dd286e3473be70de4c6e25e9342f1b3dff


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
    2c7fe4a5e0a142cf38092d1b9873a9a2a2774f0833771832316eca16ffcd95a9     2        5000000 lovelace + 990 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHashNone
    3fc7f5f649a6b53fa6202c41786968cac116177273884a15ff2fcbf32c80db94     0        192185691 lovelace + TxOutDatumHashNone
    3fc7f5f649a6b53fa6202c41786968cac116177273884a15ff2fcbf32c80db94     1        5000000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tFARM + TxOutDatumHashNone
    3fc7f5f649a6b53fa6202c41786968cac116177273884a15ff2fcbf32c80db94     2        194918428 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tCORN + 10 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHashNone
    3fc7f5f649a6b53fa6202c41786968cac116177273884a15ff2fcbf32c80db94     3        2000000 lovelace + TxOutDatumHashNone

There should be no eUTxOs at the script address.

In this example, record that we have:

    TXID_6=3fc7f5f649a6b53fa6202c41786968cac116177273884a15ff2fcbf32c80db94


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
    ad45e8a21493441dd98be853f370b2226dd89978666a92f8456b18660114949e     0        594526876 lovelace + TxOutDatumHashNone
    
    $ cardano-cli query utxo $MAGIC --address $ADDRESS_1
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------

In this example, record that we have:

    TXID_0=ad45e8a21493441dd98be853f370b2226dd89978666a92f8456b18660114949e


Simulation and PAB Examples
---------------------------

The oracle can be incorporated into other smart-contract scripts that use the oracle's value in their validation logic via the `readOracleConstraints` function in [`Mantra.Oracle.Client`](src/Mantra/Oracle/Client.hs), which returns the correct lookups, transaction constraints, and datum for a script endpoint to employ the oracle. The `readOracle` function is the simplest example of an endpoint: it just reads the oracle value and performs no other actions.

See the slighly older version of this tool at https://github.com/functionally/mantra-oracle/blob/51d21574dd2a11280ece72068d56ef33f5672404/ReadMe.md for examples use the Plutus simulator and the Plutus Application Backend. Full simulator and PAB support will be included when the PAB is released.


API documentation
-----------------

See https://functionally.github.io/mantra-oracle/ for API documentation.


Development environment
-----------------------

Due to quirks in how [`haskell.nix`](https://input-output-hk.github.io/haskell.nix/) and [`cabal.project`](https://cabal.readthedocs.io/en/3.4/cabal-project.html) interact, the following procedure needs to be followed to create a development environment for compiling `mantra`:

1.  Run `nix-shell`. This takes a while to build unless you set `withHoogle = false` in [shell.nix](shell.nix).
2.  Temporarily comment-out the `source-repository-package` lines in [cabal.project](cabal.project).
3.  Run `cabal build`, `hoogle`, or other development tools defined in [shell.nix](shell.nix).
