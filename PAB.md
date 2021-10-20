Examples Using Plutus Application Backend (PAB)
===============================================


Tracing of Emulated Execution
-----------------------------

The following command runs a trace that includes creating, reading, writing, and deleting the oracle. A transcript of the output is in [`example.log`](example.log).

    mantra-oracle -- trace Êþ CORN FARM PIGY 1000000 0

Here `CORN` is the control token, `FARM` the datum token, and `PIGY` the fee token: these are assets under the `cafe` currency symbol. The minimum fee for using the oracle in a contract is `1000000 PIGY`.


Simulation on Mock PAB
----------------------

The PAB example uses three executables:

1.  First start the simulate Plutus application backend (PAB), and leave it running. Wait until three `[INFO] Activated instance` messages are printed before running the oracle in step #2: this lets enough slots pass to create the oracle and transfer newly minted funds to the other wallet(s). In this example, `BRIO` is the control token, `SOFR` the datum token, and `PIGY` the fee token: these are assets under the a newly created currency symbol. The minimum fee for using the oracle in a contract is `1000000 PIGY`. Wallet #1, which controls the oracle, will be created with `0 PIGY` and its Contract ID (CID) will be stored in the file `oracle.cid`; wallet #2 will have `2500000 PIGY` and its CID stored in `wallet-2.cid`.

    mantra-oracle pab-simulate CORN FORM PIGY 1000000 0 '[(1, 0, "oracle.cid"), (2, 2500000, "wallet-2.cid")]'

2.  Next run the oracle data source, and leave this running. This periodically polls the data source and posts data when it changes. In this example, we poll the [New York Federal Reserve Bank SOFR API](https://markets.newyorkfed.org/api/rates/secured/sofr/last/1.json) every 300 seconds, connect to the PAB at `127.0.0.1:9080`, and call the `write` endpoint for the Contract ID (CID) in the file `oracle.cid`.

    mantra-oracle/mantra-oracle pab-control 300 127.0.0.1 9080 oracle.cid

3.  Each time we want to use the oracle data in a bare-bones contract, we can run the following. In this example, we connect to the PAB at `127.0.0.1:8080` and call the `read` endpoint for the Contract ID (CID) in the file `wallet-2.cid`. If we call the end point more than twice, reading the oracle will fail due to insufficient funds.

    mantis-oracle pab-employ 127.0.0.1 9080 wallet-2.cid
