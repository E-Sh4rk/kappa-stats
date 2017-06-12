open Printf

let file = ref ""
let outputs_prefix = ref ""
let verbose = ref false
let rule_of_interest = ref ""

let options = [
  ("-o", Arg.Set_string outputs_prefix,
   "name prefix for the output files");
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
  List.sort_uniq Agent.compare (List.flatten (List.map (List.map agent_of_test) instantiation.Instantiation.tests))

module IntMap = Map.Make(struct type t = int let compare = compare end)
type binding_type = (int*int) option
module BindingMap = Map.Make(struct type t = binding_type let compare = compare end)

let get_state_eff agent_type site state c =
  try ( Hashtbl.find c.(agent_type).(site) state)
  with Not_found -> 0

let get_link_eff agent_type site link c =
  try ( Hashtbl.find c.(agent_type).(site) link)
  with Not_found -> 0

let binding_type_of_binding b = match b with
  | None -> None
  | Some ((a_id,a_type),s) -> Some (a_type, s)

let treat_agent mixture counter_state counter_link (agent_id, agent_type) =
  let nb_sites = Edges.get_sites agent_id mixture in
  for site=0 to nb_sites-1 do
    let value = Edges.get_internal agent_id site mixture in
    Hashtbl.replace counter_state.(agent_type).(site) value ((get_state_eff agent_type site value counter_state)+1) ;
    let link = binding_type_of_binding (Edges.link_destination agent_id site mixture) in
    Hashtbl.replace counter_link.(agent_type).(site) link ((get_link_eff agent_type site link counter_link)+1)
  done

let treat_event instantiation mixture counter_state counter_link =
  let agents = agents_involved instantiation in
  List.iter (treat_agent mixture counter_state counter_link) agents

let init_counter_state signatures =
  let nb_agents = Signature.size signatures in
  let c = Array.make nb_agents (Array.of_list []) in
  for i=0 to nb_agents-1 do
    let nb_sites = Signature.arity signatures i in
    let s = Array.make nb_sites (Hashtbl.create 10) in
    c.(i) <- s
  done ; c

let init_counter_link signatures =
  let nb_agents = Signature.size signatures in
  let c = Array.make nb_agents (Array.of_list []) in
  for i=0 to nb_agents-1 do
    let nb_sites = Signature.arity signatures i in
    let s = Array.make nb_sites (Hashtbl.create 10) in
    c.(i) <- s
  done ; c

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
    let () = log "Loading the trace file." in
    let ch = open_in !file in
    let json = Yojson.Basic.from_channel ch in
    let () = close_in ch in
    let env = Model.of_yojson (Yojson.Basic.Util.member "env" json) in
    let steps = Trace.of_yojson (Yojson.Basic.Util.member "trace" json) in
    let steps_array = Array.of_list steps in
    let steps_nb = Array.length steps_array in

    if !rule_of_interest = "" then ( let () = Format.eprintf "Please specify a rule of interest." in exit 2 )
    else (
      (* For each AST left-hand pattern, for each site, associate a value to its effective. *)
      (* For each agent type, for each site, associate a value to its effective. *)
      (* Type is : (int int Hashtbl.t) array array *)
      let counter_state = init_counter_state (Model.signatures env)
      (* For each agent type, for each site, associate a binding type to its effective. *)
      (* Type is : (binding_type int Hashtbl.t) array array *)
      and counter_link = init_counter_link (Model.signatures env) in
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
      done
    )

let () = main ()
