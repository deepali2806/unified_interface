(* open Effect *)
(* type resume_result = Resume_success | Resume_failure *)

type 'a resumer = 'a -> bool
type _ Effect.t += Suspend : ('a resumer -> 'a option) -> 'a Effect.t
