open Type

let belongs_to_the_same_process_class p q =
  let b = ref false in
  p (Class (fun pc -> q (Class (fun qc -> b := (pc = qc)))));
  !b

let equal_process p q =
  let rb = ref false in
  p (ProcessId (fun pid -> q (ProcessId (fun qid -> rb := (pid = qid)))));
  !rb

let equal_state p q =
  assert (equal_process p q);
  let rb = ref false in
  p SetState; q (Equal (fun b -> rb := b)); !rb

let compare_state p q =
  assert (equal_process p q);
  let ri = ref 0 in
  p SetState; q (Compare (fun i -> ri := i)); !ri

let hash_fold_state hash_state p =
  let r = ref hash_state in
  p (Hash (!r, fun x -> r := x)); !r

let rec kequal_ps k ps qs =
  match ps, qs with
    [], [] -> k true
  | p::ps', q::qs' ->
     p SetState;
     q (Equal (fun b -> if b then kequal_ps k ps' qs' else k false))
  | _, _ -> Error.error __FUNCTION__

let equal_ps ps qs =
  let rb = ref false in
  kequal_ps (fun b -> rb := b) ps qs; !rb

let rec kcompare_ps k ps qs =
  match ps, qs with
    [], [] -> k 0
  | p::ps', q::qs' ->
     p SetState;
     q (Compare
          (fun i ->
            if i<>0 then k i else kcompare_ps k ps' qs'))
  | _, _ -> Error.error __FUNCTION__

let compare_ps ps qs =
  let ri = ref 0 in
  kcompare_ps (fun i -> ri := i) ps qs; !ri

let rec kfold k f acc xs =
  match xs with
    [] -> k acc
  | x::xs' ->
     f acc x (fun acc -> kfold k f acc xs')

let khash_ps k hash_state qs =
  kfold k
    (fun hash_state q k -> q (Hash (hash_state, k)))
    hash_state qs

let hash_fold_ps hash_state ps =
  let r = ref hash_state in
  khash_ps (fun x -> r := x) hash_state ps;
  !r

let kshow_ps k ps =
  match ps with
    [] -> k "[]"
  | p::ps' ->
     p (Show (fun s ->
            kfold
              (fun s -> k (s ^ "]"))
              (fun acc p k ->
                p (Show (fun s -> k (acc ^ ", " ^ s))))
              ("[" ^ s) ps'))

let show_ps ps =
  let rs = ref "" in
  kshow_ps (fun s -> rs := s) ps; !rs
