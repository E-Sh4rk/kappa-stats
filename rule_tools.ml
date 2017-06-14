
let srule_id_from_rule_id env rid = (Model.get_rule env rid).Primitives.syntactic_rule

let rule_ast_name env rule_id = 
  Format.asprintf "%a" 
    (Model.print_ast_rule ~env) 
    (srule_id_from_rule_id env rule_id)

let agents_involved instantiation =
  let aggregate_agent acc test = match test with
  | Instantiation.Is_Here a -> a::acc
  | _ -> acc
  in List.sort_uniq Agent.compare (List.fold_left aggregate_agent [] (List.flatten instantiation.Instantiation.tests))

let syntactic_agents_tested env rule_name =
    let rules = Model.nums_of_rule rule_name env in
    let srule_id = srule_id_from_rule_id env (List.hd rules) in
    let srule = Model.get_ast_rule env srule_id in
    srule.LKappa.r_mix

let rule_agent_site_for_link_nb rule_agent link_nb not_site =
    let ports = rule_agent.LKappa.ra_ports in
    let res = ref (-1) in
    for i=0 to (Array.length ports)-1 do
       if i<>not_site then
       (
        let ((link,locality),switching) = ports.(i) in
        match link with
            | Ast.LNK_VALUE (v, _) when v=link_nb -> res := i 
            | _ -> ()
       )
    done ; !res

let find_link_extremity rule_mixture link_nb (not_nth,not_site) =
    let rec _find_link_extremity rule_mixture nth =
    match rule_mixture with
    | [] -> (-1,-1)
    | rule_agent::lst ->
        let site = rule_agent_site_for_link_nb rule_agent link_nb (if nth=not_nth then not_site else -1) in
        if site >= 0 then (nth,site) else _find_link_extremity lst (nth+1)
    in _find_link_extremity rule_mixture 0

let agent_match_srule mixture (agent_id, agent_type) rule_mixture nth =
    let rec _agent_match_srule (agent_id, agent_type) nth links_to_ignore =
        let rule_agent = List.nth rule_mixture nth in
        if rule_agent.LKappa.ra_type <> agent_type then false
        else
        (
            let ports = rule_agent.LKappa.ra_ports
            and ints = rule_agent.LKappa.ra_ints in
            let ok = ref true in
            for i=0 to (Array.length ports) - 1 do
                let ((link,locality),switching) = ports.(i) in
                let site_link = Edges.link_destination agent_id i mixture in
                match link with
                | Ast.ANY_FREE | Ast.LNK_ANY -> ()
                | Ast.LNK_FREE -> if site_link <> None then ok := false
                | Ast.LNK_SOME -> if site_link = None then ok := false
                | Ast.LNK_TYPE (port, agent_type) ->
                (
                    match site_link with
                    | None -> ok := false
                    | Some ((dest_id,dest_type),dest_site) -> if dest_site <> port || dest_type <> agent_type then ok := false
                )
                | Ast.LNK_VALUE (v, _) ->
                (
                    if List.for_all (fun x -> x<>v) links_to_ignore then
                    (
                        match site_link with
                        | None -> ok := false
                        | Some (dest_agent,dest_site) ->
                        let (extremity_nth,extremity_site) = find_link_extremity rule_mixture v (nth,i) in
                        if extremity_site <> dest_site then ok := false
                        else
                        (
                            if not (_agent_match_srule dest_agent extremity_nth (v::links_to_ignore)) then ok := false
                        )
                    )
                )
            done ;
            for i=0 to (Array.length ints) - 1 do
                let internal = ints.(i) in
                let site_state = try Edges.get_internal agent_id i mixture with _ -> -1 in
                match internal with
                | LKappa.I_ANY | LKappa.I_ANY_CHANGED _ | LKappa.I_ANY_ERASED -> ()
                | LKappa.I_VAL_CHANGED (v,_) | LKappa.I_VAL_ERASED v -> if site_state <> v then ok := false
            done ;
            !ok
        )
    in _agent_match_srule (agent_id, agent_type) nth []

let singletons arr =
    let ret = ref [] in
    for i=0 to (Array.length arr)-1 do
        match arr.(i) with
        | [e] -> ret := e::(!ret)
        | _ -> ()
    done ; !ret

let diff_lst lst1 lst2 = List.filter (fun e -> List.for_all (fun e2 -> e<>e2) lst2) lst1

let assign_agents_instances env mixture srule_id candidate_agents =
    let srule = Model.get_ast_rule env srule_id in
    let nb_agents = List.length srule.LKappa.r_mix in
    let candidates = Array.make nb_agents [] in
    (* Deleting candidates that do not match rule_agent constraints *)
    for i=0 to nb_agents-1 do
        candidates.(i) <- List.filter (fun a -> agent_match_srule mixture a srule.LKappa.r_mix i) candidate_agents
    done ;
    (* Assign candidates to match constraints *)
    let ok = ref false in
    while not !ok do
        let sg = singletons candidates in
        ok := true ;
        for i=0 to nb_agents-1 do
            let new_lst = match diff_lst candidates.(i) sg with [] -> candidates.(i) | l -> l in
            (if new_lst <> candidates.(i) then ok := false) ;
            candidates.(i) <- new_lst
        done
    done ;
    candidates
