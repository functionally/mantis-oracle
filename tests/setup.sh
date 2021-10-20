#!/usr/bin/env bash

set -e

. config.sh


### Network.

cardano-cli query protocol-parameters $MAGIC --out-file $PROTOCOL


### Currency.

cat << EOI > $SCRIPT_M
{
  "type"   : "sig"
, "keyHash": "$(cardano-cli address key-hash --payment-verification-key-file $VERIFICATION_0)"
}
EOI


### Oracle.

mantra-oracle export $CONFIG $SCRIPT_O


### Example reader client.

mantra-oracle reader $CONFIG $SCRIPT_C


### Record transaction.

echo "Run query.sh to find the resulting transaction and enter that as TXID_0 in local.sh."
