(* open Effect *)

type 'a resumer = ('a, exn) result -> unit
type _ Effect.t += Suspend : ('a resumer -> bool) -> 'a Effect.t
