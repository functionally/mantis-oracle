
### Configuration.

CONFIG=../testnet.mantra-oracle


### Network.

MAGIC="--testnet-magic 1097911063"

PROTOCOL=../testnet.protocol


### Wallets.

ADDRESS_0=$(cat ../testnet.payment-0.address)
ADDRESS_1=$(cat ../testnet.payment-1.address)

PAYMENT_0=../testnet.payment-0.skey
PAYMENT_1=../testnet.payment-1.skey

VERIFICATION_0=../testnet.payment-0.vkey
VERIFICATION_1=../testnet.payment-1.vkey


### Scripts.

SCRIPT_M=../testnet.policy-0.script
SCRIPT_O=../testnet.plutus
SCRIPT_C=../example-reader.plutus


### Data.

DATUM_0=../example-data-0.json
DATUM_1=../example-data-1.json

