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

let treat_event instantiation mixture = ()

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
      let mixture = ref (Replay.init_state ~with_connected_components:true) in
      for i=0 to steps_nb-1 do
        let step = steps_array.(i) in
        (
        match step with
        | Rule (rule_id, instantiation, infos) when rule_ast_name env rule_id = !rule_of_interest
        -> treat_event instantiation (!mixture)
        | _ -> ()
        ) ;
        let (new_mixture, _) = Replay.do_step (Model.signatures env) (!mixture) step in
        mixture := new_mixture
      done
    )

let () = main ()
