
. config.sh
. local.sh


### Tokens.

CURRENCY=$(cardano-cli transaction policyid --script-file $SCRIPT_M)

TOKEN_C=$CURRENCY.tCORN
TOKEN_D=$CURRENCY.tFARM
TOKEN_F=$CURRENCY.tPIGY


### Oracle.

ADDRESS_S=$(cardano-cli address build $MAGIC --payment-script-file $SCRIPT_O)


### Example reader client.

ADDRESS_C=$(cardano-cli address build $MAGIC --payment-script-file $SCRIPT_C)


### Datum.

HASH_0=$(cardano-cli transaction hash-script-data --script-data-value "$(cat $DATUM_0)")
HASH_1=$(cardano-cli transaction hash-script-data --script-data-value "$(cat $DATUM_1)")


### Functions.

function assert_success() {
  if [[ $? -eq 0 ]]
  then
    echo PASSED "$1"
  else
    echo FAILED "$1"
  fi
  echo
}

function assert_failure() {
  if [[ $? -eq 0 ]]
  then
    echo FAILED "$1"
  else
    echo PASSED "$1"
  fi
  echo
}
