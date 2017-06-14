
let rule_ast_name env rule_id = 
  Format.asprintf "%a" 
    (Model.print_ast_rule ~env) 
    (Model.get_rule env rule_id).Primitives.syntactic_rule

let agents_involved instantiation =
  let aggregate_agent acc test = match test with
  | Instantiation.Is_Here a -> a::acc
  | _ -> acc
  in List.sort_uniq Agent.compare (List.fold_left aggregate_agent [] (List.flatten instantiation.Instantiation.tests))

let nb_of_syntactic_agents_tested env rule_name =
    let rules = Model.nums_of_rule rule_name env in
    let srule_id = (Model.get_rule env (List.hd rules)).Primitives.syntactic_rule in
    let srule = Model.get_ast_rule env srule_id in
    List.length (srule.LKappa.r_mix)



