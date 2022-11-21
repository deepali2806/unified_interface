open Effect

module type Base_Sched = 
  sig
    type 'a t 
    val return : 'a -> 'a t
    val suspend_fn : ('a Sched.resumer -> bool) -> 'a t
  end


module type Base_mvar = 
  sig
    type 'a t
    (* type 'a u *)
    val take_monad :  'a MVar.t -> 'a t
    val put_monad : 'a MVar.t -> 'a -> unit t
  end
  
module Make_monad (S : Base_Sched) : (Base_mvar with type 'a t = 'a S.t)
= 
struct
  type 'a t = 'a S.t
  let rec take_monad m = 
  let p  = (try S.return (MVar.take m) with
          | Unhandled (Sched.Suspend f) -> try Obj.magic (S.suspend_fn f) with
                                          | Exit -> take_monad m 
                  ) 
  in p

let rec put_monad m v = 
  let v = (try S.return (MVar.put m v) with
          | Unhandled (Sched.Suspend f) ->  try Obj.magic (S.suspend_fn f) with
                                            | Exit -> put_monad m v
          ) 
          in v
end

(* 
module Final_monad = Make_monad (struct 
                                  type 'a t = 'a Lwt.t
                                  let return = Lwt.return
                                  let suspend_fn = Lwt.suspend
                                  end
                                )  *)

