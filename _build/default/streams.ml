open Effect
open Printf

type 'a stream_state = {
  mutex : Mutex.t;
  capacity : int;
  item_q : 'a Queue.t;
  readers : 'a Sched.resumer Queue.t;
  writers : unit Sched.resumer Queue.t;
}

type 'a t = 'a stream_state

let create capacity =
  {
    mutex = Mutex.create ();
    capacity;
    item_q = Queue.create ();
    readers = Queue.create ();
    writers = Queue.create ();
  }

let add stream item =
  Mutex.lock stream.mutex;
  match Queue.pop stream.readers with
  (* Resume the resumer with value*)
  | reader_resumer ->
      if reader_resumer item then printf "Successfully resumed"
      else printf "Resumer was cancelled";
      Mutex.unlock stream.mutex
  (* REaders queue is empty  *)
  | exception Queue.Empty ->
      if Queue.length stream.item_q < stream.capacity then (
        Queue.push item stream.item_q;
        Mutex.unlock stream.mutex)
      else
        (* Add the writer in writer's queue *)
        let block r =
          Queue.push r stream.writers;
          Mutex.unlock stream.mutex;
          None
        in
        perform (Sched.Suspend block)

let take stream =
  Mutex.lock stream.mutex;
  match Queue.take_opt stream.item_q with
  | None ->
      let block r =
        Queue.push r stream.readers;
        Mutex.unlock stream.mutex;
        None
      in
      perform (Sched.Suspend block)
  | Some v ->
      (match Queue.pop stream.writers with
      | writer_resumer ->
          if writer_resumer () then
            printf "Writer resumer is resumed successfully"
          else printf "Writer resumer is cancelled somewhere else"
      | exception Queue.Empty -> ());
      Mutex.unlock stream.mutex;
      v
