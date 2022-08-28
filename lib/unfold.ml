open Type
open Primitive

let hash_state s =
  Base.Hash.get_hash_value (hash_fold_state (Base.Hash.alloc ()) s)

let unfold channel_to_event_list p =

  let ht = Hashtable.create equal_state hash_state 1000 in
  let que = Queue.create () in

  let add s path =
    let id = Hashtable.length ht in
    let b = Hashtable.add_if_not_exists ht s (id, []) in
    if b then Queue.add (s, id, path) que
  in

  let rec loop () =
    if Queue.is_empty que then () else
      let (p, id, path) = Queue.take que in
      p (Trans (fun trans_list ->
             let ts =
               List.fold_left (fun acc (trans, party) ->
                   match trans with
                     TransTau q ->
                      add q ((Tau, party)::path);
                      ((Tau, party), q)::acc
                   | TransEvent (e, q) ->
                      add q ((Event e, party)::path);
                      ((Event e, party), q)::acc
                   | TransReceive (ch, guard, targetf) ->
                      List.fold_left (fun acc e ->
                          if guard e then
                            let q = targetf e in
                            add q ((Event e, party)::path);
                            ((Event e, party), q)::acc
                          else
                            acc)
                        acc (channel_to_event_list ch))
                 [] trans_list
             in
             Hashtable.replace ht p (id, ts);
             loop ()))
  in
  add p [];
  loop ();
  ht
