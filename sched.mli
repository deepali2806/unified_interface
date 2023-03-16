(* open Effect *)
type resume_result = Resume_success | Resume_failure

type 'a resumer = 'a -> resume_result
type _ Effect.t += Suspend : ('a resumer -> bool) -> 'a Effect.t
