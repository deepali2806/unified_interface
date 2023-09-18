open Effect
open Sched

type 'a state = Full of 'a | Empty of 'a resumer list
type 'a t = 'a state Atomic.t

let create () = Atomic.make (Empty [])

let take p =
  let rec block r =
    let old = Atomic.get p in
    match old with
    | Full v -> Some v
    | Empty l ->
        if Atomic.compare_and_set p old (Empty (r :: l)) then None else block r
  in
  let old = Atomic.get p in
  match old with Full v -> v | Empty _ -> perform (Suspend block)

exception Already_filled

let rec put p v =
  let old = Atomic.get p in
  match old with
  | Full _ -> raise Already_filled
  | Empty l ->
      if Atomic.compare_and_set p old (Full v) then
        List.iter (fun r -> if r (Ok v) then ()) l
      else put p v
