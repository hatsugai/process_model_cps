open Printf
open CspModel
open CspModel.Type
open CspModel.Primitive
open CspModel.Sequential
open CspModel.Composition
open Base.Hash.Builtin          (* @_@ *)

type event = In of int | Mid of int | Out of int
[@@deriving show { with_path=false }, eq, ord, hash]

type channel = ChIn | ChMid | ChOut
[@@deriving show { with_path=false }, eq, ord]

type p_state = P1 | P2 of int | P3 of int
[@@deriving show { with_path=false }, eq, ord, hash]

type q_state = Q1 | Q2 of int | Q3
[@@deriving show { with_path=false }, eq, ord, hash]

let p_transf s pk =
  match s with
    P1 ->
     [
       TransReceive
         (ChIn,
          (fun e -> true),
          (fun e ->
            match e with
              In x -> pk (P2 x)
            | _ -> Error.error __FUNCTION__));
     ]
  | P2 x -> [TransTau (pk (P3 x))]
  | P3 x -> [TransEvent (Mid x, pk P1)]

let q_transf s pk =
  match s with
    Q1 ->
     [
       TransReceive (ChMid, (fun e -> true),
                     (fun e ->
                       match e with
                         Mid x -> pk (Q2 x)
                       | _ -> Error.error "Ch"));
     ]
   | Q2 x -> [TransEvent (Out x, (pk Q3))]
   | Q3 -> [TransTau (pk Q1)]

let e_to_ch e =
  match e with
  | In _ -> ChIn
  | Mid _ -> ChMid
  | Out _ -> ChOut

let sync ch = (ch = ChMid)

let rec iota a b =
  if a >= b then [] else a::(iota (a+1) b)

let channel_to_event_list ch =
  match ch with
    ChIn -> List.map (fun x -> In x) (iota 0 2)
  | ChMid-> List.map (fun x -> Mid x) (iota 0 3)
  | ChOut -> List.map (fun x -> Out x) (iota 0 3)

let () =
  let p_class =
    make_process_class equal_p_state hash_fold_p_state compare_p_state show_p_state
      P1                    (* P1 is just a filler, not the initial state *)
  in
  let p = make_process p_class p_transf P1 in (* P1 is the initial state *)
  let q_class =
    make_process_class equal_q_state hash_fold_q_state compare_q_state show_q_state
      Q1
  in
  let q = make_process q_class q_transf Q1 in
  let s_class = make_composite_process_class () in
  let s = concurrent_composition equal_event ~process_class:s_class e_to_ch sync [p; q] in
  let ht = CspModel.Unfold.unfold channel_to_event_list p in
  CspModel.Vis.vis "p" p_class show_event ht;
  let ht = CspModel.Unfold.unfold channel_to_event_list q in
  CspModel.Vis.vis "q" q_class show_event ht;
  let ht = CspModel.Unfold.unfold channel_to_event_list s in
  CspModel.Vis.vis "s" s_class show_event ht;
  ()
