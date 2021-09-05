#!/usr/bin/env bash

. env.sh

echo


### Fail to create because of missing tokens.

mantra-oracle create $CONFIG $ADDRESS_0 $PAYMENT_0 $DATUM_0 2>/dev/null
assert_failure "01a No creation without tokens."


### Fail to create because of incorrect signing key.

mantra-oracle create $CONFIG $ADDRESS_1 $PAYMENT_0 $DATUM_0 2>/dev/null           
assert_failure "01b No creation with bad key."


### Create.

mantra-oracle create $CONFIG $ADDRESS_1 $PAYMENT_1 $DATUM_0            
assert_success "01c Creation."


### Record transaction.

echo "Run query.sh to find the resulting transaction and enter that as TXID_2 in local.sh."
