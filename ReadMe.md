The Mantra Oracle: A General-Purpose Token-Based Oracle for Cardano
===================================================================

This Cardano oracle reports structured data (namely, the `PlutuxTx.BuiltinData` type) to a transaction if the fee, as a quantity of a fungible token, is paid. It can be incorporated into other smart-contract scripts that use the oracle's value in their validation logic.


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

*   Version 1.29.0-rc2 of `cardano-node` and the bundled version of `cardano-cli`.
*   `alonzo-purple.payment-0.address` contains the address of a wallet funded with test ADA.
*   `alonzo-purple.payment-0.skey` contains the corresponding signing key for the above address.
*   `alonzo-purple.payment-0.vkey` contains the corresponding verification key for the above address.
*   `alonzo-purple.payment-1.address` contains the address of a (possibly empty) address for use in the example.
*   `alonzo-purple.payment-1.skey` contains the corresponding signing key for the above address.
*   The control token for the oracle is named `tBRIO`.
*   The datum token for the oracle is named `tSOFR`.
*   The fee token for the oracle is named `tPIGY`.
*   Five of the `tPIGY` tokens are needed to read the oracle, but no additional ADA is needed, aside from the transaction fee.
*   The `mantra-oracle` executable program is on the search path, given by the `PATH` environment variable.


### Set up the network.

    MAGIC="--testnet-magic 8"
    
    cardano-cli query protocol-parameters $MAGIC --out-file alonzo-purple.protocol


### Record the test addresses.

    ADDRESS_0=$(cat alonzo-purple.payment-0.address); echo $ADDRESS_0
    
    ADDRESS_1=$(cat alonzo-purple.payment-1.address); echo $ADDRESS_1


### Create the minting policy.

    cat << EOI > alonzo-purple.policy-0.script
    {
        "type" : "sig"
    ,   "keyHash" : "$(cardano-cli address key-hash --payment-verification-key-file alonzo-purple.payment-0.vkey)"
    }
    EOI
    
    CURRENCY=$(cardano-cli transaction policyid --script-file alonzo-purple.policy-0.script); echo $CURRENCY


### Configure the command-line tool.

We use a configuration file like [alonzo-purple.mantra-oracle](alonzo-purple.mantra-oracle). Modify the path to the local node socket, and modify policy ID for the three assets to match `$CURRENCY`.

    Configuration
    {
      socketPath     = "/data/alonzo.socket"
    , magic          = Just 8
    , epochSlots     = 43200
    , controlAsset   = "13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tBRIO"
    , datumAsset     = "13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tSOFR"
    , feeAsset       = "13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY"
    , feeAmount      = 5
    , lovelaceAmount = 0
    }


### Create the script address.

First, learn about the `export` command:

    $ mantra-oracle export --help
    
    Usage: mantra-oracle export CONFIG_FILE OUTPUT_FILE
    
      Export the validator code and compute its address.
    
    Available options:
      CONFIG_FILE              The configuration file.
      OUTPUT_FILE              Output filename for the serialized validator.
      -h,--help                Show this help text

Now export the Plutus script for the oracle and find its address:

    SCRIPT_FILE=alonzo-purple.plutus; echo $SCRIPT_FILE
    
    mantra-oracle export alonzo-purple.mantra-oracle $SCRIPT_FILE
    
    ADDRESS_S=$(cardano-cli address build $MAGIC --payment-script-file $SCRIPT_FILE); echo $ADDRESS_S


### Find a UTxO with test ADA.

    $ cardano-cli query utxo $MAGIC --address $ADDRESS_0
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    96023be874b18db04c0591790e9e873101c433c7f5ecdffd1ac0210618ddc85a     0        998054618860 lovelace + TxOutDatumHashNone

In this example, record that we have:

    TXID_0=96023be874b18db04c0591790e9e873101c433c7f5ecdffd1ac0210618ddc85a


### Mint and distribute the tokens.

    cardano-cli transaction build $MAGIC --alonzo-era \
      --tx-in $TXID_0#0 \
      --tx-out "$ADDRESS_1+100000000" \
      --tx-out "$ADDRESS_1+100000000+1000 $CURRENCY.tPIGY" \
      --tx-out "$ADDRESS_1+100000000+1 $CURRENCY.tSOFR" \
      --tx-out "$ADDRESS_1+100000000+1 $CURRENCY.tBRIO" \
      --tx-out "$ADDRESS_1+2000000" \
      --change-address $ADDRESS_0 \
      --mint "1000 $CURRENCY.tPIGY+1 $CURRENCY.tSOFR+1 $CURRENCY.tBRIO" \
      --mint-script-file alonzo-purple.policy-0.script \
      --out-file tx.raw
    
    cardano-cli transaction sign $MAGIC \
      --tx-body-file tx.raw \
      --out-file tx.signed \
      --signing-key-file alonzo-purple.payment-0.skey
    
    cardano-cli transaction submit $MAGIC \
      --tx-file tx.signed

Wait until the transaction is recorded on the blockchain and lookup the eUTxOs.

    $ cardano-cli query utxo $MAGIC --address $ADDRESS_1
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    00e347e6ccfc43472155729cfd35297ad924c4f65cf576f34727f3f85d5c16cf     1        100000000 lovelace + TxOutDatumHashNone
    00e347e6ccfc43472155729cfd35297ad924c4f65cf576f34727f3f85d5c16cf     2        100000000 lovelace + 1000 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHashNone
    00e347e6ccfc43472155729cfd35297ad924c4f65cf576f34727f3f85d5c16cf     3        100000000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tSOFR + TxOutDatumHashNone
    00e347e6ccfc43472155729cfd35297ad924c4f65cf576f34727f3f85d5c16cf     4        100000000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tBRIO + TxOutDatumHashNone
    00e347e6ccfc43472155729cfd35297ad924c4f65cf576f34727f3f85d5c16cf     5        2000000 lovelace + TxOutDatumHashNone

In this example, record that we have:

    TXID_1=00e347e6ccfc43472155729cfd35297ad924c4f65cf576f34727f3f85d5c16cf


### Create the oracle.

First, learn about the `create` command:

    $ mantra-oracle -- create --help

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

    mantra-oracle create alonzo-purple.mantra-oracle     \
                  $(cat alonzo-purple.payment-1.address) \
                  alonzo-purple.payment-1.skey           \
                  example-data-0.json

Wait until the transaction is recorded on the blockchain and look at the eUTxOs:

    $ cardano-cli query utxo $MAGIC --address $ADDRESS_S
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    2a28a432202a0303a6ffda1a33820f1d24dd4d44719f737993f363f1d031f154     1        5000000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tSOFR + TxOutDatumHash ScriptDataInAlonzoEra "27f7dc5d54a5a5cfa25f75f200f62f5a3decce6ca05f4497ce3123d8b3f682ad"
    
    $ cardano-cli query utxo $MAGIC --address $ADDRESS_1
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    00e347e6ccfc43472155729cfd35297ad924c4f65cf576f34727f3f85d5c16cf     2        100000000 lovelace + 1000 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHashNone
    2a28a432202a0303a6ffda1a33820f1d24dd4d44719f737993f363f1d031f154     0        146298199 lovelace + TxOutDatumHashNone
    2a28a432202a0303a6ffda1a33820f1d24dd4d44719f737993f363f1d031f154     2        148500000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tBRIO + TxOutDatumHashNone
    2a28a432202a0303a6ffda1a33820f1d24dd4d44719f737993f363f1d031f154     3        2000000 lovelace + TxOutDatumHashNone

In this example, record that we have:

    TXID_2=2a28a432202a0303a6ffda1a33820f1d24dd4d44719f737993f363f1d031f154

The datum hash in the eUTxO should match the hash for the file:

    $ cardano-cli transaction hash-script-data --script-data-value "$(cat example-data-0.json)"
    
    27f7dc5d54a5a5cfa25f75f200f62f5a3decce6ca05f4497ce3123d8b3f682ad


### Read the oracle.

Reading the oracle is typically done by another smart contract that needs to use the data in the oracle's eUTxO. For this example, we just use `cardano-cli` to read the oracle.

    cardano-cli transaction build $MAGIC --alonzo-era \
      --protocol-params-file alonzo-purple.protocol \
      --tx-in $TXID_2#1 \
        --tx-in-script-file $SCRIPT_FILE \
        --tx-in-datum-value "$(cat example-data-0.json)" \
        --tx-in-redeemer-value '1' \
      --tx-in $TXID_1#2 \
      --tx-out "$ADDRESS_S+5000000+1 $CURRENCY.tSOFR+5 $CURRENCY.tPIGY" \
        --tx-out-datum-hash $(cardano-cli transaction hash-script-data --script-data-value '"Hello PIGY!"') \
      --tx-out "$ADDRESS_1+5000000+995 $CURRENCY.tPIGY" \
      --change-address $ADDRESS_1 \
      --tx-in-collateral $TXID_2#3 \
      --out-file tx.raw
    
    cardano-cli transaction sign $MAGIC \
      --tx-body-file tx.raw \
      --out-file tx.signed \
      --signing-key-file alonzo-purple.payment-1.skey
    
    cardano-cli transaction submit $MAGIC \
      --tx-file tx.signed

Wait until the transaction is recorded on the blockchain and look at the eUTxOs:

    $ cardano-cli query utxo $MAGIC --address $ADDRESS_S
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    fc33770afe5ba4d1020c6b806aaf9d67804aeff7e7d50d3b281fe9c425d628d6     1        5000000 lovelace + 5 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tSOFR + TxOutDatumHash ScriptDataInAlonzoEra "27f7dc5d54a5a5cfa25f75f200f62f5a3decce6ca05f4497ce3123d8b3f682ad"
    
    $ cardano-cli query utxo $MAGIC --address $ADDRESS_1
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    2a28a432202a0303a6ffda1a33820f1d24dd4d44719f737993f363f1d031f154     0        146298199 lovelace + TxOutDatumHashNone
    2a28a432202a0303a6ffda1a33820f1d24dd4d44719f737993f363f1d031f154     2        148500000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tBRIO + TxOutDatumHashNone
    2a28a432202a0303a6ffda1a33820f1d24dd4d44719f737993f363f1d031f154     3        2000000 lovelace + TxOutDatumHashNone
    fc33770afe5ba4d1020c6b806aaf9d67804aeff7e7d50d3b281fe9c425d628d6     0        94330916 lovelace + TxOutDatumHashNone
    fc33770afe5ba4d1020c6b806aaf9d67804aeff7e7d50d3b281fe9c425d628d6     2        5000000 lovelace + 995 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHashNone

You can see that the oracle has acquired the `5 tPIGY` fee, but no additional ADA.

In this example, record that we have:

    TXID_3=fc33770afe5ba4d1020c6b806aaf9d67804aeff7e7d50d3b281fe9c425d628d6


### Write new data to the oracle.

First, learn about the `write` command:

    $ mantra-oracle -- write --help
    
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

    mantra-oracle write alonzo-purple.mantra-oracle      \
                  $(cat alonzo-purple.payment-1.address) \
                  alonzo-purple.payment-1.skey           \
                  example-data-0.json                    \
                  example-data-1.json

Wait until the transaction is recorded on the blockchain and look at the eUTxOs:

    $ cardano-cli query utxo $MAGIC --address $ADDRESS_S
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    9f47a4730f1de22082571d584ccb9f234ee6101f44d8e61943d78fce8549a135     1        5000000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tSOFR + TxOutDatumHash ScriptDataInAlonzoEra "000448b2cf8aeea0eac22d1e7a5cb6dd286e3473be70de4c6e25e9342f1b3dff"
    
    $ cardano-cli query utxo $MAGIC --address $ADDRESS_1
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    9f47a4730f1de22082571d584ccb9f234ee6101f44d8e61943d78fce8549a135     0        192715018 lovelace + TxOutDatumHashNone
    9f47a4730f1de22082571d584ccb9f234ee6101f44d8e61943d78fce8549a135     2        195564558 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tBRIO + 5 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHashNone
    9f47a4730f1de22082571d584ccb9f234ee6101f44d8e61943d78fce8549a135     3        2000000 lovelace + TxOutDatumHashNone
    fc33770afe5ba4d1020c6b806aaf9d67804aeff7e7d50d3b281fe9c425d628d6     2        5000000 lovelace + 995 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHashNone

In this example, record that we have:

    TXID_4=9f47a4730f1de22082571d584ccb9f234ee6101f44d8e61943d78fce8549a135

The datum hash in the eUTxO will match the hash for the new file:

    $ cardano-cli transaction hash-script-data --script-data-value "$(cat example-data-1.json)"
    
    000448b2cf8aeea0eac22d1e7a5cb6dd286e3473be70de4c6e25e9342f1b3dff


### Delete the oracle.

First, learn about the `delete` command:

    $ mantra-oracle -- delete --help
    
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

    mantra-oracle delete alonzo-purple.mantra-oracle     \
                  $(cat alonzo-purple.payment-1.address) \
                  alonzo-purple.payment-1.skey           \
                  example-data-1.json

Wait until the transaction is recorded on the blockchain and look at the eUTxOs:

    $ cardano-cli query utxo $MAGIC --address $ADDRESS_S
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    
    $ cardano-cli query utxo $MAGIC --address $ADDRESS_1
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    5099217a3557354cb940b9316360b6ee3780ded34e6766775134595d200dffe0     0        192381022 lovelace + TxOutDatumHashNone
    5099217a3557354cb940b9316360b6ee3780ded34e6766775134595d200dffe0     1        5000000 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tSOFR + TxOutDatumHashNone
    5099217a3557354cb940b9316360b6ee3780ded34e6766775134595d200dffe0     2        195139788 lovelace + 1 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tBRIO + 5 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHashNone
    5099217a3557354cb940b9316360b6ee3780ded34e6766775134595d200dffe0     3        2000000 lovelace + TxOutDatumHashNone
    fc33770afe5ba4d1020c6b806aaf9d67804aeff7e7d50d3b281fe9c425d628d6     2        5000000 lovelace + 995 13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1.tPIGY + TxOutDatumHashNone

There should be no eUTxOs at the script address.

In this example, record that we have:

    TXID_5=5099217a3557354cb940b9316360b6ee3780ded34e6766775134595d200dffe0


### Clean up the example so it can be run again.

We can burn the test tokens and collect the remaining test ADA from this example.

    cardano-cli transaction build $MAGIC --alonzo-era \
      --tx-in $TXID_1#0 \
      --tx-in $TXID_3#2 \
      --tx-in $TXID_5#0 \
      --tx-in $TXID_5#1 \
      --tx-in $TXID_5#2 \
      --tx-in $TXID_5#3 \
      --change-address $ADDRESS_0 \
      --mint "-1000 $CURRENCY.tPIGY+-1 $CURRENCY.tSOFR+-1 $CURRENCY.tBRIO" \
      --mint-script-file alonzo-purple.policy-0.script \
      --out-file tx.raw
    
    cardano-cli transaction sign $MAGIC \
      --tx-body-file tx.raw \
      --out-file tx.signed \
      --signing-key-file alonzo-purple.payment-0.skey \
      --signing-key-file alonzo-purple.payment-1.skey
    
    cardano-cli transaction submit $MAGIC \
      --tx-file tx.signed

Wait until the transaction is recorded on the blockchain and look at the eUTxOs:

    $ cardano-cli query utxo $MAGIC --address $ADDRESS_S
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    
    $ cardano-cli query utxo $MAGIC --address $ADDRESS_0
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    d4d1e4f08d621988295f8999c01511672cc4e0e422e9c12c72aa02f7068306b6     0        998051751292 lovelace + TxOutDatumHashNone
    
    $ cardano-cli query utxo $MAGIC --address $ADDRESS_1
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------

In this example, record that we have:

    TXID_0=d4d1e4f08d621988295f8999c01511672cc4e0e422e9c12c72aa02f7068306b6


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
