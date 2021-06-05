A General-Purpose Token-Based Oracle for Cardano
================================================

This Cardano oracle reports structured data (namely, the `PlutuxTx.Data` type) to a transaction if the fee, as a quantity of a fungible token, is paid. It can be incorporated into other smart-contract scripts that use the oracle's value in their validation logic.


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
      }

*   The `controlParameter` specifies a native token the that controller (owner) of the oracle uses to update the oracle's value, withdraw funds, or close (delete) the oracle.
*   The `datumParameter` specifies the non-fungible token that identifies the UTxO containing the oracle data.
*   The quantity `feeAmount` of the fungible token `feeToken` must be paid to the oracle in order to read its value in an on-chain validator.


Actions
-------

The oracle can perform three simple actions:

    data Action =
        Delete
      | Read
      | Write

*   The `Read` action simply reads the value of the oracle into a transaction, and corresponds to the `read` endpoint in [`Mantis.Oracle.Client`](src/Mantis/Oracle/Client.hs). The required fee must be paid to the oracle script, and the UTxO containing the oracle data must be consumed and paid back to the oracle script, with the data unchanged.
*   The `Write` action updates the value of the oracle, and corresponds to the `write` endpoint in [`Mantis.Oracle.Controller`](src/Mantis/Oracle/Controller.hs). The control token (specified by `controlParameter`) must be present in the transaction, and the UTxO containing the oracle data must be consumed and paid back (with the revised data) to the oracle script.
*   The `Delete` action shuts down the oracle by removing the data and associated NFT, and corresponds to the `delete` endpoint in [`Mantis.Oracle.Controller`](src/Mantis/Oracle/Controller.hs). The control token must be present in the transaction.


Usage
-----

The oracle can be incorporated into other smart-contract scripts that use the oracle's value in their validation logic via the `readOracleConstraints` function in [`Mantis.Oracle.Client`](src/Mantis/Oracle/Client.hs), which returns the correct lookups, transaction constraints, and datum for a script endpoint to employ the oracle. The `readOracle` function is the simplest example of an endpoint: it just reads the oracle value and performs no other actions.

![Example transactions using the oracle.](transactions.png)


Example Using Trace
-------------------

The following command runs a trace that includes creating, reading, writing, and deleting the oracle. A transcript of the output is in [`example.log`](example.log).

    mantis-oracle trace cafe BRIO SOFR PIGY 1000000

Here `BRIO` is the control token, `SOFR` the datum token, and `PIGY` the fee token: these are assets under the `cafe` currency symbol. The minimum fee for using the oracle in a contract is `1000000 PIGY`.


Example Using Plutus Application Backend (PAB)
----------------------------------------------

The PAB example uses three executables:

1.  First start the simulate Plutus application backend (PAB), and leave it running. Wait until three `[INFO] Activated instance` messages are printed before running the oracle in step #2: this lets enough slots pass to create the oracle and transfer newly minted funds to the other wallet(s). In this example, `BRIO` is the control token, `SOFR` the datum token, and `PIGY` the fee token: these are assets under the `cafe` currency symbol. The minimum fee for using the oracle in a contract is `1000000 PIGY`. Wallet #1, which controls the oracle, will be created with `0 PIGY` and its Contract ID (CID) will be stored in the file `oracle.cid`; wallet #2 will have `2500000 PIGY` and its CID stored in `wallet-2.cid`.
    
    mantis-oracle simulate BRIO SOFR PIGY 1000000 '[(1, 0, "oracle.cid"), (2, 2500000, "wallet-2.cid")]'
    
2.  Next run the oracle data source, and leave this running. This periodically polls the data source and posts data when it changes. In this example, we poll the [SOFR oracle](src/Mantis/Oracle/SOFR.hs) every 3600 seconds, connect to the PAB at `127.0.0.1:8080`, and call the `write` endpoint for the Contract ID (CID) in the file `oracle.cid`.
    
    mantis-oracle control 3600 127.0.0.1 8080 oracle.cid
    
3.  Each time we want to use the oracle data in a bare-bones contract, we can run the following. In this example, we connect to the PAB at `127.0.0.1:8080` and call the `read` endpoint for the Contract ID (CID) in the file `wallet-2.cid`. If we call the end point too many times, reading the oracle will fail due to insufficient funds.
    
    mantis-oracle employ 127.0.0.1 8080 wallet-2.cid
    
