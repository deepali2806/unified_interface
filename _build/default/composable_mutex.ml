open Effect

  type state = Unlocked | Locked of unit Sched.resumer list
  type t = state Atomic.t

  let create () = Atomic.make Unlocked

  let lock m =
    let rec block r =
      let old = Atomic.get m in
      match old with
      | Unlocked -> if Atomic.compare_and_set m old (Locked [])
					then (Some ()) else block r (* failed CAS; retry *)

      | Locked l -> if Atomic.compare_and_set m old (Locked (r::l))
          then None else block r (* failed CAS; retry *)
    in
    perform (Sched.Suspend block)

  let rec unlock m =
    let old = Atomic.get m in
    match old with
    | Unlocked -> failwith "impossible"
    | Locked [] -> if Atomic.compare_and_set m old Unlocked
        then () else unlock m (* failed CAS; retry *)
    | Locked (r::rs) -> if Atomic.compare_and_set m old (Locked rs)
        then begin
          if r () then () (* successfully transferred control *)
          else unlock m (* cancelled; wake up next task *)
        end else unlock m (* failed CAS; retry *)
