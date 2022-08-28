open Printf
open Type

let show_event_ex show_event ex =
  match ex with
    Tau -> "tau"
  | Event e -> show_event e

let vis name process_class show_event ht =
  let emit_states ch =
    Hashtable.iter
      (fun s (id, _) ->
        let name = s SetState; process_class.show process_class.state in
        fprintf ch "%d [label=\"%d\\n%s\"];\n" id id name)
      ht
  in
  let emit_transitions ch =
    let emit_tr pid ((e, (_ : party)), q) =
      match Hashtable.find_opt ht q with
        None -> Error.error __FUNCTION__
      | Some (qid, _) ->
         fprintf ch "%d -> %d [label=\"%s\"];\n" pid qid
           (show_event_ex show_event e)
    in
    Hashtable.iter
      (fun _ (id, ts) ->
        List.iter (emit_tr id) ts)
      ht

  in
  let ch = open_out (name ^ ".dot") in
  fprintf ch "digraph {\n";
  fprintf ch "node [fontname = \"Menlo\",fontsize=10];\n";
  fprintf ch "edge [fontname = \"Menlo\",fontsize=10];\n";
  emit_states ch;
  emit_transitions ch;
  fprintf ch "}\n";
  close_out ch;
  let command = sprintf "dot -Tsvg -o %s.svg %s.dot" name name in
  let _ = Unix.system command in
  let command = sprintf "dot -Tpdf -o %s.pdf %s.dot" name name in
  let _ = Unix.system command in
  ()
