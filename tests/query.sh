#!/usr/bin/env bash

set -e

. env.sh

for a in $ADDRESS_0 $ADDRESS_1 $ADDRESS_S $ADDRESS_C
do
  echo
  echo
  echo $a
  echo
  cardano-cli query utxo $MAGIC --address $a
done
