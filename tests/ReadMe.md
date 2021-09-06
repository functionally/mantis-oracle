Tests for the Oracle's Plutus Validator
=======================================

The test suite provides complete coverage for the logical and redemption of the Plutus validator for the oracle. These 29 tests are only semi-automated, so the scripts need to be run one at a time.

1.  Edit [config.sh](config.sh) to set filenames etc.
3.  Run [setup.sh](setup.sh) to create the various necessary files.
2.  Run [query.sh](query.sh) and edit `TXID_0` in [local.sh](local.sh) so that it points to the source of funds in the first payment address.
4.  Run [00-mint.sh](00-mint.sh) to mint the coins needed for the tests.
5.  Run [query.sh](query.sh) and edit `TXID_1` in [local.sh](local.sh) to record the minting transaction.
6.  Run [01-create.sh](01-create.sh) and check for failures.
7.  Run [query.sh](query.sh) and edit `TXID_2` in [local.sh](local.sh) to record the creation transaction.
8.  Run [02-read.sh](02-read.sh) and check for failures.
9.  Run [query.sh](query.sh) and edit `TXID_3` in [local.sh](local.sh) to record the reading transaction.
10. Run [03-write.sh](03-write.sh) and check for failures.
11. Run [query.sh](query.sh) and edit `TXID_4` in [local.sh](local.sh) to record the writing transaction.
12. Run [04-other.sh](04-other.sh) and check for failures.
13. Run [05-delete.sh](05-delete.sh) and check for failures.
14. Run [query.sh](query.sh) and edit `TXID_5` in [local.sh](local.sh) to record the deletion transaction.
15. Run [99-burn.sh](99-burn.sh) and check for failures.
