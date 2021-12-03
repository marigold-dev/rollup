open Helpers
open Crypto

module Address_and_ticket_map = struct
  type key = { address : Address.t; ticket : Ticket.t } [@@deriving ord, yojson]

  module Map = Map.Make_with_yojson (struct
    type t = key [@@deriving ord, yojson]
  end)

  type t = Amount.t Map.t [@@deriving yojson]

  let empty = Map.empty
  let find_opt address ticket = Map.find_opt { address; ticket }
  let add address ticket = Map.add { address; ticket }
end
module Handle = struct
  type t = {
    hash : BLAKE2B.t;
    id : int;
    owner : Tezos.Address.t;
    amount : Amount.t;
    ticket : Ticket.t;
  }
  [@@deriving yojson]

  let hash ~id ~owner ~amount ~ticket =
    let Ticket.{ ticketer; data } = ticket in
    Tezos.Rollup.Consensus.hash_withdraw_handle ~id:(Z.of_int id) ~owner
      ~amount:(Z.of_int (Amount.to_int amount))
      ~ticketer ~data
end
module Handle_tree = Incremental_patricia.Make (struct
  type t = Handle.t [@@deriving yojson]

  let hash t = t.Handle.hash
end)

type t = { ledger : Address_and_ticket_map.t; handles : Handle_tree.t }
[@@deriving yojson]

let empty =
  { ledger = Address_and_ticket_map.empty; handles = Handle_tree.empty }

let balance address ticket t =
  Address_and_ticket_map.find_opt address ticket t.ledger
  |> Option.value ~default:Amount.zero

let assert_available ~source ~(amount : Amount.t) =
  if source >= amount then Ok () else Error `Not_enough_funds

let transfer ~source ~destination amount ticket t =
  let open Amount in
  let open Result.Syntax in
  let source_balance = balance source ticket t in
  let* () = assert_available ~source:source_balance ~amount in

  let destination_balance = balance destination ticket t in

  Ok
    {
      ledger =
        t.ledger
        |> Address_and_ticket_map.add source ticket (source_balance - amount)
        |> Address_and_ticket_map.add destination ticket
             (destination_balance + amount);
      handles = t.handles;
    }

let deposit destination amount ticket t =
  let open Amount in
  let destination_balance = balance destination ticket t in
  {
    ledger =
      t.ledger
      |> Address_and_ticket_map.add destination ticket
           (destination_balance + amount);
    handles = t.handles;
  }

let withdraw ~source ~destination amount ticket t =
  let open Amount in
  let open Result.Syntax in
  let owner = destination in
  let source_balance = balance source ticket t in
  let* () = assert_available ~source:source_balance ~amount in
  let handles, handle =
    Handle_tree.add
      (fun id ->
        let hash = Handle.hash ~id ~owner ~amount ~ticket in
        { id; hash; owner; amount; ticket })
      t.handles
  in
  let t =
    {
      ledger =
        t.ledger
        |> Address_and_ticket_map.add source ticket (source_balance - amount);
      handles;
    }
  in
  Ok (t, handle)

let handles_find_proof handle t =
  match Handle_tree.find handle.Handle.id t.handles with
  (* TODO: enforce this unreachability on the type system
     the only way to have a Handle.t is to do a withdraw *)
  | None -> assert false
  | Some (proof, _) -> proof

let handles_find_proof_by_id key t = Handle_tree.find key t.handles
let handles_root_hash t = Handle_tree.hash t.handles
