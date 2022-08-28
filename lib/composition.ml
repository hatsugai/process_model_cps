open Type
open Primitive

let rec cartesian_product xss =
  match xss with
    [] -> [[]]
  | xs::xss' ->
     List.fold_left
       (fun acc ys ->
         List.fold_left
           (fun acc x -> (x::ys)::acc)
           acc xs)
       [] (cartesian_product xss')

let make_composite_process_class () = {
    equal =  equal_ps;
    hash_fold_t = hash_fold_ps;
    compare = compare_ps;
    show = show_ps;
    state = [];
  }

let concurrent_composition
      ?(process_class : ('e, 'ch) process list process_class = make_composite_process_class ())
      (equal_event : 'e -> 'e -> bool)
      (e_to_ch : 'e -> 'ch)
      (sync : 'ch -> bool)
      (ps : ('e, 'ch) process list) : ('e, 'ch) process =

  let n = List.length ps in

  let process_descriptor = ref 0 in

  let rec transf (ps : ('e, 'ch) process list) k =

    let ht = Hashtbl.create 0 in

    let reg ch i (trans : ('e, 'ch) trans) party =
      let v =
        match Hashtbl.find_opt ht ch with
          Some v -> v
        | None ->
           let v = Array.make n [] in
           Hashtbl.replace ht ch v; v
      in
      v.(i) <- (trans, party)::v.(i)         
    in

    let consensus ch (ys : (('e, 'ch) trans * party) list) : (('e, 'ch) trans * party) option =

      let rec loop_ev e rs ps (ys : (('e, 'ch) trans * party) list) =
        match ys with
          [] ->
           let qs = List.rev rs in
           let ps = List.rev ps in
           Some (TransEvent (e, mk_pk qs), Sync ps)
        | (y, party)::ys' ->
           (match y with
              TransTau _ -> Error.error ""
            | TransEvent (e', q) ->
               if equal_event e e' then
                 loop_ev e (q::rs) (party::ps) ys'
               else
                 None
            | TransReceive (_, g, tf) ->
               if g e then
                 loop_ev e ((tf e)::rs) (party::ps) ys'
               else
                 None)

      and loop_recv gs rs ps ys =
        match ys with
          [] ->
           let ts = List.rev rs in
           let ps = List.rev ps in
           Some (TransReceive (ch,
                          (fun e -> List.for_all (fun g -> g e) gs),
                          (fun e ->
                            let qs = List.map (fun tf -> tf e) ts in
                            mk_pk qs)),
             Sync ps)
        | (y, party)::ys' ->
           (match y with
              TransTau _ -> Error.error ""
            | TransEvent (e, q) ->
               if List.for_all (fun g -> g e) gs then
                 loop_ev e (q::(List.map (fun tf -> tf e) rs)) (party::ps) ys'
               else
                 None
            | TransReceive (_, g, tf) ->
               loop_recv (g::gs) (tf::rs) (party::ps) ys')
      in

      match ys with
        [] -> Error.error ""
      | (y, party)::ys' ->
         (match y with
            TransTau _ -> Error.error ""
          | TransEvent (e, q) -> loop_ev e [q] [party] ys'
          | TransReceive (_, g, tf) -> loop_recv [g] [tf] [party] ys')
    in

    let sync_trans acc =
      let acc =
        Hashtbl.fold
          (fun ch v acc ->
            let xss = Array.to_list v in
            let yss = cartesian_product xss in
            List.fold_left
              (fun acc ys ->
                match consensus ch ys with
                  None -> acc
                | Some t -> t::acc)
              acc yss)
          ht acc
      in
      k acc
    in

    let rec loop (acc : (('e, 'ch) trans * party) list) i (rs : ('e, 'ch) process list)
              (ps : ('e, 'ch) process list) =
      match ps with
        [] -> sync_trans acc
      | p::ps' ->
         p
           (Trans
              (fun (trans_list : (('e, 'ch) trans * party) list) ->
                let acc =
                  List.fold_left
                    (fun acc (trans, party) ->
                      match trans with
                        TransTau q ->
                         let qs = List.rev_append rs (q::ps') in
                         (TransTau (mk_pk qs), Interleave (i, party)) :: acc
                      | TransEvent (e, q) ->
                         let ch = e_to_ch e in
                         if sync ch then
                           (reg ch i trans party; acc)
                         else
                           let qs = List.rev_append rs (q::ps') in
                           (TransEvent (e, (mk_pk qs)), Interleave (i, party)) :: acc
                      | TransReceive (ch, guard, targetf) ->
                         if sync ch then
                           (reg ch i trans party; acc)
                         else
                           (TransReceive (ch, guard,
                                     (fun e ->
                                       let qs = List.rev_append rs ((targetf e)::ps') in
                                       mk_pk qs)),
                            Interleave (i, party))
                           :: acc)
                    acc trans_list
                in
                loop acc (i+1) (p::rs) ps'))

    in
    loop [] 0 [] ps

  and mk_pk qs cmd =
    match cmd with
      Trans k -> transf qs k
    | Class k -> k (ObjectAddress.f process_class)
    | ProcessId k -> k (ObjectAddress.f process_descriptor)
    | SetState -> process_class.state <- qs
    | Equal k -> kequal_ps k process_class.state qs
    | Compare k -> kcompare_ps k process_class.state qs
    | Hash (hash_state, k) -> khash_ps k hash_state qs
    | Anatomy k -> k (Some qs)
    | Show k -> kshow_ps k qs

  in
  mk_pk ps
