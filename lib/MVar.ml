open Effect

type 'a mv_state =
  | Full of 'a * ('a * unit Sched.resumer) Fun_queue.t
  | Empty of 'a Sched.resumer Fun_queue.t

type 'a t = 'a mv_state Atomic.t

let create_empty () = Atomic.make (Empty Fun_queue.empty)
let create v = Atomic.make (Full (v, Fun_queue.empty))

let rec put mv v =
  let old_contents = Atomic.get mv in
  match old_contents with
  | Full _ ->
      let rec block r =
        let old_contents = Atomic.get mv in
        match old_contents with
        | Full (v', q) ->
            let newQueue = Fun_queue.push q (v, r) in
            let new_contents = Full (v', newQueue) in
            if Atomic.compare_and_set mv old_contents new_contents then None
            else block r
        | Empty q -> (
            if Fun_queue.length q = 0 then
              let new_contents = Full (v, Fun_queue.empty) in
              if Atomic.compare_and_set mv old_contents new_contents then
                Some ()
              else block r
            else
              match Fun_queue.pop q with
              | None -> Some ()
              | Some (x, newQueue) ->
                  let resume = x in
                  let new_contents = Empty newQueue in
                  if Atomic.compare_and_set mv old_contents new_contents then
                    if resume (Ok v) then Some ()
                    else (* Retrying to get the next task*)
                      block r
                  else block r)
      in
      perform (Sched.Suspend block)
  | Empty q -> (
      if Fun_queue.length q = 0 then (
        let new_contents = Full (v, Fun_queue.empty) in
        let ret = Atomic.compare_and_set mv old_contents new_contents in
        if ret == false then put mv v)
      else
        match Fun_queue.pop q with
        | None -> ()
        | Some (x, newQueue) ->
            let resume = x in
            let new_contents = Empty newQueue in
            let ret = Atomic.compare_and_set mv old_contents new_contents in
            if ret then
              if resume (Ok v) then ()
              else (* Retrying to get the next task*)
                put mv v
            else put mv v)

let rec take mv =
  let old_contents = Atomic.get mv in
  match old_contents with
  | Empty _ ->
      let rec block r =
        let old_contents = Atomic.get mv in
        match old_contents with
        | Empty q ->
            let newQueue = Fun_queue.push q r in
            let new_contents = Empty newQueue in
            if Atomic.compare_and_set mv old_contents new_contents then None
            else block r
        | Full (v, q) -> (
            if Fun_queue.length q = 0 then
              let new_contents = Empty Fun_queue.empty in
              let ret = Atomic.compare_and_set mv old_contents new_contents in
              if ret then Some v else block r
            else
              match Fun_queue.pop q with
              | None -> raise Exit
              | Some ((v', resume), newQueue) ->
                  let new_contents = Full (v', newQueue) in
                  let ret =
                    Atomic.compare_and_set mv old_contents new_contents
                  in
                  if ret then
                    let ret1 = resume (Ok ()) in
                    if ret1 then Some v else block r
                  else block r)
      in
      perform (Sched.Suspend block)
  | Full (v, q) -> (
      if Fun_queue.length q = 0 then
        let new_contents = Empty Fun_queue.empty in
        let ret = Atomic.compare_and_set mv old_contents new_contents in
        if ret then v else take mv
      else
        match Fun_queue.pop q with
        | None -> raise Exit
        | Some ((v', resume), newQueue) ->
            let new_contents = Full (v', newQueue) in
            let ret = Atomic.compare_and_set mv old_contents new_contents in
            if ret then
              let ret1 = resume (Ok ()) in
              if ret1 then v else take mv
            else take mv)
