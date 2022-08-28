open Type

let make_process
      (process_class : 'state process_class)
      transf
      (initial_state : 'state)
    : ('event, 'channel) process =
  let pid = ObjectAddress.f (ref ()) in
  let rec pk s cmd =
    match cmd with
      Trans k -> k (List.map (fun t -> (t, Alone)) (transf s pk))
    | Class k -> k (ObjectAddress.f process_class)
    | ProcessId k -> k pid
    | SetState -> process_class.state <- s
    | Equal k -> k (process_class.equal s process_class.state)
    | Hash (hash_state, k) -> k (process_class.hash_fold_t hash_state s)
    | Compare k -> k (process_class.compare s process_class.state)
    | Anatomy k -> k None
    | Show k -> k (process_class.show s)
  in
  pk initial_state
