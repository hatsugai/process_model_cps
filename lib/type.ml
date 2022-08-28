type 'e event_ex = Tau | Event of 'e
[@@deriving show { with_path=false }, eq, ord, hash]

type party =
  Alone
| Interleave of int * party
| Sync of party list
| Annihilation
[@@deriving show { with_path=false }, eq, ord]

type hash_state = Base.Hash.state

type ('e, 'ch) process = ('e, 'ch) command -> unit
[@printer fun fmt state -> state (Show (fun s -> fprintf fmt "%s" s))]
[@@deriving show { with_path=false }]

and ('e, 'ch) command =
  Trans of ((('e, 'ch) trans * party) list -> unit)
| Class of (ObjectAddress.t -> unit)
| ProcessId of (ObjectAddress.t -> unit)
| SetState
| Equal of (bool -> unit)
| Compare of (int -> unit)
| Hash of (hash_state [@opaque]) * ((hash_state [@opaque]) -> unit)
| Anatomy of (('e, 'ch) process list option -> unit)
| Show of (string -> unit)
[@@deriving show { with_path=false }, eq, ord]

and ('e, 'ch) trans =
  TransTau of ('e, 'ch) process
| TransEvent of 'e * ('e, 'ch) process
| TransReceive of 'ch * 'e guard * ('e, 'ch) targetf
[@@deriving show { with_path=false }, eq, ord]

and 'e guard = ('e -> bool)
[@printer fun fmt _ -> fprintf fmt "<guard>"]
[@@deriving show { with_path=false }, eq, ord]

and ('e, 'ch) targetf = ('e -> ('e, 'ch) process)
[@printer fun fmt _ -> fprintf fmt "<targetf>"]
[@@deriving show { with_path=false }, eq, ord]

type 'state process_class = {
    equal : 'state -> 'state -> bool;
    hash_fold_t : hash_state -> 'state -> hash_state;
    compare : 'state -> 'state -> int;
    show : 'state -> string;
    mutable state : 'state;
  }

type ('e, 'ch) composite_process_class = ('e, 'ch) process list process_class

let make_process_class equal hash_fold_t compare show state =
  { equal; hash_fold_t; compare; show; state }
