type 'a t

val create : unit -> 'a t

exception Already_filled

val put : 'a t -> 'a -> unit
val take : 'a t -> 'a
