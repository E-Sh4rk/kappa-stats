
let file = ref ""
let output_file = ref ""
let show_all = ref false
let rule_of_interest = ref ""

let options = [
  ("-o", Arg.Set_string output_file,
   "name for the output file");
  ("-r", Arg.Set_string rule_of_interest,
   "rule of interest");
  ("--all", Arg.Set show_all,
   "show also tested sites")
]

let description = ""
let log s = print_string s ; print_newline () ; flush stdout

module IntMap = Map.Make(struct type t = int let compare = compare end)
type binding_type = (int*int) option
module BindingMap = Map.Make(struct type t = binding_type let compare = compare end)

let get_eff i site state c =
  try ( Hashtbl.find c.(i).(site) state)
  with Not_found -> 0

let binding_type_of_binding b = match b with
  | None -> None
  | Some ((a_id,a_type),s) -> Some (a_type, s)

let treat_agent env mixture counter_state counter_link i (agent_id, agent_type) =
  let nb_sites = Edges.get_sites agent_id mixture (*Signature.arity signatures agent_type*) in
  for site=0 to nb_sites-1 do
    if !show_all || not (Rule_tools.is_internal_state_tested env (!rule_of_interest) i site) then
    (
      try (
      let value = Edges.get_internal agent_id site mixture in
      Hashtbl.replace counter_state.(i).(site) value ((get_eff i site value counter_state)+1) ;
      ) with _ -> ()
    )
    ;
    if !show_all || not (Rule_tools.is_link_type_tested env (!rule_of_interest) i site) then
    (
      let link = binding_type_of_binding (Edges.link_destination agent_id site mixture) in
      Hashtbl.replace counter_link.(i).(site) link ((get_eff i site link counter_link)+1)
    )
  done

let treat_event env srule_id instantiation mixture counter_state counter_link =
  let agents = Rule_tools.assign_agents_instances env mixture srule_id (Rule_tools.agents_involved instantiation) in
  let agents = Rule_tools.choose_possible_configuration agents in
  Array.iteri (treat_agent env mixture counter_state counter_link) agents

let init_counter env rule_name =
  let agents_tested = Rule_tools.syntactic_agents_tested env rule_name in
  let nb_agents = List.length agents_tested in
  let c = Array.make nb_agents (Array.of_list []) in
  for i=0 to nb_agents-1 do
    let nb_sites = Signature.arity (Model.signatures env) (List.nth agents_tested i).LKappa.ra_type in
    let s = Array.make nb_sites (Hashtbl.create 0) in
    for j=0 to nb_sites-1 do
      s.(j) <- Hashtbl.create 3
    done ;
    c.(i) <- s
  done ; c

let compute_elem htbl =
  let eff = Hashtbl.fold (fun k v acc -> acc + v) htbl 0 in
  let tuples = Hashtbl.fold (fun k v acc -> (k,(float_of_int v)/.(float_of_int eff))::acc) htbl [] in
  List.sort (fun (k,v) (k',v') -> compare v' v) tuples
let compute_data c =
  Array.map (Array.map compute_elem) c

let print_binding_type signature fmt bt =
  let pr x = Format.fprintf fmt x in match bt with
  | None -> pr "Free"
  | Some (agent_type, site) ->
    Signature.print_agent signature fmt agent_type ;
    pr "." ; Signature.print_site signature agent_type fmt site

let print_state signature fmt agent_id site (i,v) =
  let pr x = Format.fprintf fmt x in
  let name = Signature.internal_state_of_num site i (Signature.get signature agent_id) in
  pr "%s (%d%%) ; " name (int_of_float (v*.100.0))

let print_link signature fmt agent_id site (link_type,v) =
  let pr x = Format.fprintf fmt x in
  print_binding_type signature fmt link_type ;
  pr " (%d%%) ; " (int_of_float (v*.100.0))

let print_site signature fmt agent_id site (states, links) =
  let pr x = Format.fprintf fmt x in
  let name = Signature.site_of_num site (Signature.get signature agent_id) in
  if states <> [] then (
    pr "%s internal states : " name ;
    List.iter (print_state signature fmt agent_id site) states ;
    pr "\n"
  ) ;
  if links <> [] then (
    pr "%s bindings : " name ;
    List.iter (print_link signature fmt agent_id site) links ;
    pr "\n"
  ) ;
  if states <> [] || links <> [] then pr "@."

let print_agent fmt signature agent_id (states, links) =
  let pr x = Format.fprintf fmt x in
  let stats = Array.map2 (fun a b -> (a,b)) states links in
  (*let name = Signature.agent_of_num i signatures in
  pr "%s :@." name ;*)
  pr "---------- " ;
  Signature.print_agent signature fmt agent_id ;
  pr " ----------@." ;
  Array.iteri (print_site signature fmt agent_id) stats ;
  pr "@."

let write_output env rule_name states links =
  let signature = Model.signatures env in
  let agents_tested = Rule_tools.syntactic_agents_tested env rule_name in
  let oc = open_out !output_file in
  let fmt = Format.formatter_of_out_channel oc in
  let stats = Array.map2 (fun a b -> (a,b)) states links in
  Array.iteri (fun i s -> print_agent fmt signature (List.nth agents_tested i).LKappa.ra_type s) stats ; close_out oc

let main () =
  let () =
    Arg.parse
      options
      (fun f -> if !file = "" then file := f else
          let () = Format.eprintf "Deals only with 1 file" in exit 2)
      description in
  if !file = "" then
    prerr_string "Please specify a trace file."
  else
  (
    log "Loading the trace file." ;
    let ch = open_in !file in
    let json = Yojson.Basic.from_channel ch in
    let () = close_in ch in
    let env = Model.of_yojson (Yojson.Basic.Util.member "env" json) in
    let steps = Trace.of_yojson (Yojson.Basic.Util.member "trace" json) in
    log "Trace file loaded !" ;

    if !output_file = "" then output_file := !file^".log" ;
    if !rule_of_interest = "" then ( let () = Format.eprintf "Please specify a rule of interest." in exit 2 )
    else (
      try
      (
        (* For each AST left-hand agent pattern, for each site, associate a value to its effective. *)
        (* Type is : (int int Hashtbl.t) array array *)
        let counter_state = init_counter env !rule_of_interest
        (* For each AST left-hand agent pattern, for each site, associate a binding type to its effective. *)
        (* Type is : (binding_type int Hashtbl.t) array array *)
        and counter_link = init_counter env !rule_of_interest in
        let steps_tail = ref steps in
        let mixture = ref (Replay.init_state ~with_connected_components:true) in
        for i=0 to (List.length steps)-1 do
          let step = List.hd (!steps_tail) in
          steps_tail := List.tl (!steps_tail) ;
          (
          match step with
          | Trace.Rule (rule_id, instantiation, infos) when Rule_tools.rule_ast_name env rule_id = !rule_of_interest
          -> treat_event env (Rule_tools.srule_id_from_rule_id env rule_id) instantiation (!mixture).Replay.graph counter_state counter_link
          | _ -> ()
          ) ;
          let (new_mixture, _) = Replay.do_step (Model.signatures env) (!mixture) step in
          mixture := new_mixture
        done ;
        write_output env !rule_of_interest (compute_data counter_state) (compute_data counter_link) ;
        log "Done !"
      )
      with _ -> log "Invalid rule of interest !"
    ) 
  )

let () = main ()
