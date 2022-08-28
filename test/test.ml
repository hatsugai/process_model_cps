open Printf
open CspModel
open CspModel.Type
open CspModel.Primitive
open CspModel.Sequential
open CspModel.Composition
open Base.Hash.Builtin

type event = EvA | EvB | EvC | Ch of int | Ch2 of int
[@@deriving show { with_path=false }, eq, ord, hash]

type channel = ChA | ChB | ChC | ChCh | ChCh2
[@@deriving show { with_path=false }, eq, ord, hash]

type p_state = P1 | P2 | P3 | P4 | P5
[@@deriving show { with_path=false }, eq, ord, hash]

type q_state = Q1 | Q2 | Q3 | Q4 of int | Q5
[@@deriving show { with_path=false }, eq, ord, hash]

let p_transf s pk =
  match s with
    P1 ->
     [
       TransEvent (EvA, pk P2);
       TransEvent (EvB, pk P2);
       TransEvent (EvB, pk P3);
       TransEvent (Ch 314, pk P4);
       TransReceive (ChCh2, (fun e -> true), (fun e -> pk P5));
     ]
  | _ ->
     [TransTau (pk P1)]

let q_transf s pk =
  match s with
    Q1 ->
     [
       TransEvent (EvA, pk Q2);
       TransEvent (EvC, pk Q3);
       TransEvent (EvC, pk Q5);
       TransReceive (ChB, (fun e -> true), (fun e -> pk Q2));
       TransReceive (ChB, (fun e -> true), (fun e -> pk Q3));
       TransReceive (ChCh, (fun e -> true),
                     (fun e ->
                       match e with
                         Ch k -> pk (Q4 k)
                       | _ -> Error.error "Ch"));
       TransReceive (ChCh2, (fun e -> true), (fun e -> pk Q5));
     ]
  | _ ->
     [TransTau (pk Q1)]

let print_trans x =
  printf "%s" (show_trans pp_event pp_channel x)

let e_to_ch e =
  match e with
    EvA -> ChA
  | EvB -> ChB
  | EvC -> ChC
  | Ch _ -> ChCh
  | Ch2 _ -> ChCh2

let sync ch = (ch = ChB || ch = ChCh || ch = ChCh2)

let print_state s =
  s (Show (fun s -> printf "%s\n" s))

let print_trans_party (trans, party) =
  print_trans trans;
  print_string " | ";
  print_string (show_party party);
  print_string "\n"

let f s =
  printf "\n--------------------\n";
  print_state s;
  s (Trans
       (fun trans_list ->
         List.iter print_trans_party trans_list))

let () =
  let p_class = make_process_class equal_p_state hash_fold_p_state compare_p_state show_p_state P1 in
  let p = make_process p_class p_transf P1 in
  let q_class = make_process_class equal_q_state hash_fold_q_state compare_q_state show_q_state Q1 in
  let q = make_process q_class q_transf Q1 in
  let s = concurrent_composition equal_event e_to_ch sync [p; q] in
  let w = concurrent_composition equal_event e_to_ch sync [s; q] in
  f p;
  f q;
  f s;
  f w
