open Effect
open Printf

type 'a stream_state = {
  mutex : Mutex.t;
  capacity : int;
  item_q : ('a, exn) Result.t Queue.t;
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

let rec add stream item =
  Mutex.lock stream.mutex;
  match Queue.pop stream.readers with
  (* Resume the resumer with value*)
  | reader_resumer ->
      if reader_resumer (Ok item) then printf "Successfully resumed"
      else printf "Resumer was cancelled";
      Mutex.unlock stream.mutex;
      (* Retrying to skip the cancelled resumer *)
      add stream item
  (* REaders queue is empty  *)
  | exception Queue.Empty ->
      if Queue.length stream.item_q < stream.capacity then (
        Queue.push (Ok item) stream.item_q;
        Mutex.unlock stream.mutex)
      else
        let block resumer =
          let resumer v =
            if Result.is_ok v then Queue.push (Ok item) stream.item_q;
            resumer v
          in
          Queue.push resumer stream.writers;
          Mutex.unlock stream.mutex;
          None
        in
        perform (Sched.Suspend block)

let rec take stream =
  Mutex.lock stream.mutex;
  match Queue.take_opt stream.item_q with
  | None ->
      let block r =
        Queue.push r stream.readers;
        Mutex.unlock stream.mutex;
        None
      in
      perform (Sched.Suspend block)
  | Some v -> (
      match Queue.pop stream.writers with
      | writer_resumer ->
          if writer_resumer (Ok ()) then (
            printf "Writer resumer is resumed successfully";
            Mutex.unlock stream.mutex;
            match v with Ok v -> v | Error exn -> raise exn)
          else (
            printf "Writer resumer is cancelled somewhere else";
            Mutex.unlock stream.mutex;
            take stream (* Retrying to skip the cancelled resumer *))
      | exception Queue.Empty -> (
          ();
          Mutex.unlock stream.mutex;
          match v with Ok v -> v | Error exn -> raise exn))
