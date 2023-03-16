open Effect

exception Abort of string

type 'a mv_state =
  | Full  of 'a * ('a * unit Sched.resumer) Fun_queue.t
  | Empty of 'a Sched.resumer Fun_queue.t

type 'a t = 'a mv_state Atomic.t

let create_empty () = Atomic.make (Empty (Fun_queue.empty))

let create v = Atomic.make (Full (v, Fun_queue.empty))


let rec put mv v =
  let old_contents = Atomic.get (mv) in
  match old_contents with
  | Full (v', q) -> let p = ref true in 
                    (try perform (Sched.Suspend (fun r -> 
                                            let newQueue = Fun_queue.push q (v,r) in
                                            let new_contents = Full (v', newQueue) in
                                            p := Atomic.compare_and_set mv old_contents new_contents;
                                            !p
                                            )) 
                    with
                    | Exit -> put mv v)
  | Empty q ->
      if Fun_queue.length q = 0 then 
                  begin
                    let new_contents = Full (v, Fun_queue.empty) in
                    let ret = Atomic.compare_and_set mv old_contents new_contents in 
                    if (ret == false) then 
                      put mv v
                  end     
      else
          match Fun_queue.pop q with
                          | None -> ()
                          | Some (x, newQueue) -> let resume = x in
                                                  let new_contents = Empty newQueue in
                                                  let ret = Atomic.compare_and_set mv old_contents new_contents in 
                                                  if ret then
                                                    begin
                                                      (* resume (Ok v);
                                                      () *)
                                                      (* This was added for cancellation purpose ::==> *)
                                                      let ret1 = resume (v) in
                                                      if ret1 = Resume_success then ()
                                                      else 
                                                      (* Retrying *)
                                                        put mv v
                                                      (* raise (Abort "Excception in Put because it is already aborted")   *)
                                                    end                                               
                                                  else
                                                    put mv v

        

let rec take mv = 
  let old_contents = Atomic.get mv in 
  match old_contents with
  | Empty q -> let p = ref true in 
                  (try perform (Sched.Suspend (fun r -> 
                                            let newQueue = Fun_queue.push q r in
                                            let new_contents = Empty newQueue in
                                            p := Atomic.compare_and_set mv old_contents new_contents;
                                            !p 
                                          )
                        )
                  with Exit -> 
                  (*Printf.printf "\nInside Exit of take%!";*)
                                  take mv )
                             
  | Full (v, q) ->
                if Fun_queue.length q = 0 then
                  begin
                    let new_contents = Empty Fun_queue.empty in
                    let ret = Atomic.compare_and_set mv old_contents new_contents in 
                    if ret then 
                      v
                    else 
                      take mv
                  end 
                else
                    match Fun_queue.pop q with
                    | None -> raise (Abort "Control will never reach here, Exception in take when popping from empty queue")
                    | Some ((v', resume), newQueue) -> 
                                                  let new_contents = Full (v', newQueue) in
                                                  let ret = Atomic.compare_and_set mv old_contents new_contents in 
                                                  if ret then
                                                    begin
                                                      (* resume (Ok ());
                                                      v *)
                                                      let ret1 = resume ( ()) in 
                                                      if ret1 = Resume_success then v
                                                      else take mv
                                                      (* raise (Abort "Excception in Take because it is already aborted")    *)
                                                    end
                                                  else
                                                    take mv                 

