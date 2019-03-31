type ('nonterminal, 'terminal) symbol = | N of 'nonterminal | T of 'terminal;;

let rec subset a b =
	match a with [] -> true | head::body -> (List.mem head b) && (subset body b);;
	
let equal_sets a b = (subset a b) && (subset b a);;

let set_union a b = a @ b;;

let set_intersection a b = 
	List.filter (fun x -> List.mem x b) a;;
	
let set_diff a b =
	List.filter (fun x -> not (List.mem x b)) a;;

let rec computed_fixed_point eq f x = 
	if eq (f x) x then x else computed_fixed_point eq f (f x);;

(* get a list of all the lefthand-side nodes that a list of righthand-side leads to*)
let branches g node_data =
	List.filter (fun (x) -> List.mem (N (fst x)) node_data) g;;

(* flattens a list of lists of righthand-side data into a single list of righthand-side data*)
let rhs branches_list = 
	List.flatten (List.map (fun x -> snd(x)) branches_list);;

(* traversal: get a list of particular lefthand-side from the righthand-side, remove from g, and then recurse with the list of the righthand-side of the children; includes base case of terminate if empty*)
let rec lhs g rhs_list = 
	match (branches g rhs_list) with | [] -> g | layer -> (lhs (set_diff g layer) (rhs layer));;

(* final set of operations to get a tuple containing all the reachable rules*)
let rec filter_reachable g = (fst g, (set_diff (snd g) (lhs (snd g) ([N (fst g)]))));;
	
	
	
	