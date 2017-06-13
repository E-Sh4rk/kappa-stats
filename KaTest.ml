
let file = ref ""
let outputs_prefix = ref ""
let verbose = ref false
let rule_of_interest = ref ""

let options = [
  ("-o", Arg.Set_string outputs_prefix,
   "name for the output file");
  ("-r", Arg.Set_string rule_of_interest,
   "rule of interest");
  ("--verbose", Arg.Set verbose,
   "print annotated dot files")
]

let description = ""
let log s = print_string s ; print_newline () ; flush stdout

let rule_ast_name env rule_id = 
  Format.asprintf "%a" 
    (Model.print_ast_rule ~env) 
    (Model.get_rule env rule_id).Primitives.syntactic_rule

let agent_of_test test = match test with
  | Instantiation.Is_Here a -> a
  | Instantiation.Has_Internal ((a,_),_) -> a
  | Instantiation.Is_Free (a,_) -> a
  | Instantiation.Is_Bound (a, _) -> a
  | Instantiation.Has_Binding_type ((a,_),_) -> a
  | Instantiation.Is_Bound_to ((a,_),(a',_)) -> a

let agents_involved instantiation =
  List.sort_uniq Agent.compare (List.map agent_of_test (List.flatten instantiation.Instantiation.tests))

module IntMap = Map.Make(struct type t = int let compare = compare end)
type binding_type = (int*int) option
module BindingMap = Map.Make(struct type t = binding_type let compare = compare end)

let get_eff agent_type site state c =
  try ( Hashtbl.find c.(agent_type).(site) state)
  with Not_found -> 0

let binding_type_of_binding b = match b with
  | None -> None
  | Some ((a_id,a_type),s) -> Some (a_type, s)

let treat_agent mixture counter_state counter_link (agent_id, agent_type) =
  let nb_sites = Edges.get_sites agent_id mixture (*Signature.arity signatures agent_type*) in
  for site=0 to nb_sites-1 do
    (
    try (
    let value = Edges.get_internal agent_id site mixture in
    Hashtbl.replace counter_state.(agent_type).(site) value ((get_eff agent_type site value counter_state)+1) ;
    ) with _ -> ()
    ) ;
    let link = binding_type_of_binding (Edges.link_destination agent_id site mixture) in
    Hashtbl.replace counter_link.(agent_type).(site) link ((get_eff agent_type site link counter_link)+1)
  done

let treat_event instantiation mixture counter_state counter_link =
  let agents = agents_involved instantiation in
  List.iter (treat_agent mixture counter_state counter_link) agents

let init_counter signatures =
  let nb_agents = Signature.size signatures in
  let c = Array.make nb_agents (Array.of_list []) in
  for i=0 to nb_agents-1 do
    let nb_sites = Signature.arity signatures i in
    let s = Array.make nb_sites (Hashtbl.create 0) in
    for j=0 to nb_sites-1 do
      s.(j) <- Hashtbl.create 3
    done ;
    c.(i) <- s
  done ; c

let compute_elem htbl =
  let eff = Hashtbl.fold (fun k v acc -> acc + v) htbl 0 in
  Hashtbl.fold (fun k v acc -> (k,(float_of_int v)/.(float_of_int eff))::acc) htbl []
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
  pr "%s -> %d%%@." name (int_of_float (v*.100.0))

let print_link signature fmt agent_id site (link_type,v) =
  let pr x = Format.fprintf fmt x in
  print_binding_type signature fmt link_type ;
  pr " -> %d%%@." (int_of_float (v*.100.0))

let print_site signature fmt agent_id site (states, links) =
  let pr x = Format.fprintf fmt x in
  let name = Signature.site_of_num site (Signature.get signature agent_id) in
  pr "%s :@." name ;
  List.iter (print_state signature fmt agent_id site) states ;
  List.iter (print_link signature fmt agent_id site) links

let print_agent fmt signature i (states, links) =
  let pr x = Format.fprintf fmt x in
  let stats = Array.map2 (fun a b -> (a,b)) states links in
  (*let name = Signature.agent_of_num i signatures in
  pr "%s :@." name ;*)
  pr "---------- " ;
  Signature.print_agent signature fmt i ;
  pr " ----------@." ;
  Array.iteri (print_site signature fmt i) stats ;
  pr "@."

let write_output signature states links =
  let oc = open_out !outputs_prefix in
  let fmt = Format.formatter_of_out_channel oc in
  let stats = Array.map2 (fun a b -> (a,b)) states links in
  Array.iteri (print_agent fmt signature) stats ; close_out oc

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
    let steps_array = Array.of_list steps in
    let steps_nb = Array.length steps_array in
    log "Trace file loaded !" ;

    if !rule_of_interest = "" then ( let () = Format.eprintf "Please specify a rule of interest." in exit 2 )
    else (
      (* For each AST left-hand pattern, for each site, associate a value to its effective. *)
      (* For each agent type, for each site, associate a value to its effective. *)
      (* Type is : (int int Hashtbl.t) array array *)
      let counter_state = init_counter (Model.signatures env)
      (* For each agent type, for each site, associate a binding type to its effective. *)
      (* Type is : (binding_type int Hashtbl.t) array array *)
      and counter_link = init_counter (Model.signatures env) in
      let mixture = ref (Replay.init_state ~with_connected_components:true) in
      for i=0 to steps_nb-1 do
        let step = steps_array.(i) in
        (
        match step with
        | Trace.Rule (rule_id, instantiation, infos) when rule_ast_name env rule_id = !rule_of_interest
        -> treat_event instantiation (!mixture).Replay.graph counter_state counter_link
        | _ -> ()
        ) ;
        let (new_mixture, _) = Replay.do_step (Model.signatures env) (!mixture) step in
        mixture := new_mixture
      done ;
      write_output (Model.signatures env) (compute_data counter_state) (compute_data counter_link)
    ) 
  )

let () = main ()
