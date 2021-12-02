"use strict";

const fs = require("fs");
const taquito = require("@taquito/taquito");
const rpc = require("@taquito/rpc");
const { TezosToolkit } = require("@taquito/taquito");

/**
 * @typedef Input
 * @type {object}
 * @property {string} rpc_node
 * @property {number} confirmation
 * @property {string} destination
 */
/** @returns {Input} */
const input = () => JSON.parse(fs.readFileSync(process.stdin.fd));

const error = (err) => {
  console.error(err);
  process.exit();
};

/**
 * @typedef Transaction
 * @type {object}
 * @property {string} hash
 * @property {number} index
 * @property {string} entrypoint
 * @property {rpc.MichelsonV1Expression} value
 */
/** @param {Transaction} transaction */
const output = (transaction) => {
  const callback = (err) => err && error(err);
  process.stdout.write(JSON.stringify(transaction), callback);
  process.stdout.write("\n", callback);
};

const { rpc_node, confirmation, destination } = input();

const Tezos = new TezosToolkit(rpc_node);

const blockstream = Tezos.stream.subscribe("head");

blockstream.on("error", error);
blockstream.on("data", async (content) => {
  /** @type {rpc.OperationContentsAndResultMetadataTransaction} */
  const block_data = await Tezos.rpc.getBlock({block: content});
  try {
    const operations =
    block_data.operations
    .flat()
    .flatMap(x => {
        return x.contents.map(y => ({...y, hash: x.hash}))
      })
    .filter(x => {
      return x.destination === destination && x.kind === taquito.OpKind.TRANSACTION
    })
    .map(({parameters, hash}, index) => ({...parameters, index, hash, level: block_data.header.level}))
    operations.forEach(output);
  } catch (err) {
    console.error(err);
    // TODO: what happens here? I feel like this is a noop
  }
});
