#! /bin/bash

# ./sandbox.sh tear-down
# ./sandbox.sh setup

tezos_client() {
  docker exec -it my-sandbox tezos-client "$@"
}

while true; do
  tezos_client transfer 0 \
    from myWallet \
    to rollup --entrypoint 'fork_game' --arg 'Pair "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb"' --burn-cap 10
done
