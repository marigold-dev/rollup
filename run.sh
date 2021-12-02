#! /bin/bash

./sandbox.sh tear-down
./sandbox.sh setup

tezos_client() {
  docker exec -it my-sandbox tezos-client "$@"
}

while true; do
  tezos_client transfer 0 \
    from myWallet \
    to rollup --entrypoint 'commit' --arg 'Pair 1 0x5555' --burn-cap 10
done