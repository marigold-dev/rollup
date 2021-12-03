#! /bin/bash

set -e

RPC_NODE=http://localhost:20000

# This secret key never changes.
SECRET_KEY="edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq"

DATA_DIRECTORY="data"

ROLLUP=$(esy x which rollup)

rollup() {
  eval $ROLLUP '"$@"'
}

tezos-client() {
  docker exec -it my-sandbox tezos-client "$@"
}

ligo() {
  docker run --rm -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo:0.28.0 "$@"
}

message() {
  echo "=========== $@ ==========="
}

create_new_rollup () {
  message "Deploying new rollup contract"

  storage="unit"
  rollup_contract="./tezos_interop/rollup.mligo"
  storage=$(ligo compile storage "$rollup_contract" "$storage")
  contract=$(ligo compile contract $rollup_contract)

  message "Originating contract"
  sleep 2
  tezos-client --endpoint $RPC_NODE originate contract "rollup" \
    transferring 0 from myWallet \
    running "$contract" \
    --init "$storage" \
    --burn-cap 2 \
    --force

  message "Getting contract address"
  TEZOS_ROLLUP_ADDRESS="$(tezos-client --endpoint $RPC_NODE show known contract rollup | grep KT1 | tr -d '\r')"
  echo "rollup address: $TEZOS_ROLLUP_ADDRESS" 
}

tear-down() {
  if [[ $(docker ps | grep my-sandbox) ]]; then
    docker kill my-sandbox
    rm -rf "$DATA_DIRECTORY/**"
    echo "Stopped the sandbox and wiped all state."
  fi
}

start_node() {
  tear-down
  message "Starting sandbox"
  docker run --rm --name my-sandbox --detach -p 20000:20000 \
    tqtezos/flextesa:20210602 granabox start
  sleep 3
  message "Sandbox started"
  message "Configuring Tezos client"
  tezos-client --endpoint $RPC_NODE bootstrapped
  tezos-client --endpoint $RPC_NODE config update
  tezos-client --endpoint $RPC_NODE import secret key myWallet "unencrypted:$SECRET_KEY" --force
}

help() {
  # FIXME: fix these docs
  echo "TODO: "
}

case "$1" in
setup)
  start_node
  create_new_rollup
  message "Warning"
  echo "This script creates a sandbox node and is for development purposes only."
  echo "It does unsafe things like lowering the required Tezos confirmations to limits unreasonable for production."
  message "Do not use these settings in production!"
  ;;
tear-down)
  tear-down
  ;;
*)
  help
  ;;
esac
