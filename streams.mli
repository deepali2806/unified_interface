type 'a t

val create : int -> 'a t
val add : 'a t -> 'a -> unit
val take : 'a t -> 'a
