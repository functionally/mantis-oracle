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


Example
-------

See [`app/Main.hs`](app/Main.hs) for a simple example of creating, reading, writing, and deleting the oracle. A transcript of the output is in [`example.log`](example.log).
