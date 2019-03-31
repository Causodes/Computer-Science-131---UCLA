type ('nonterminal, 'terminal) symbol = | N of 'nonterminal | T of 'terminal;;

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal;;

  (* Convert Grammar *)
  
let rec search g first = match g with [] -> [] | head::tail -> if (fst head) = first then (snd head)::(search tail first) else (search tail first);;

let convert_grammar gram1 = ((fst gram1), fun x -> search (snd gram1) x);;

 (* Parse Tree Leaves *)
	
let rec breadth = function [] -> [] 
	| head::tail -> (tall head)@(breadth tail) 
and tall = function Leaf x -> [x] 
	| Node (_, y) -> breadth y;; 
	
let parse_tree_leaves tree = match tree with (Leaf x) -> [x] 
	| Node (_,y) -> breadth y;;

 (* Make Matcher *)

let rec width rtn_func rule_list accept frag =
	match rule_list with 
		| [] -> None
		| head::tail -> (match depth rtn_func head accept frag with
			| None -> width rtn_func tail accept frag
			| x -> x)
and depth rtn_func rule accept frag = 
	match rule with
		| [] -> accept frag
		| _ -> (match frag with
			| [] -> None
			| head::tail -> (match rule with
				| [] -> None
				| (T term)::rhs -> if head = term then (depth rtn_func rhs accept tail) else None
				| (N nterm)::rhs -> (width rtn_func (rtn_func nterm) (depth rtn_func rhs accept) frag)));;
				
let make_matcher gram accept frag = (width (snd gram) ((snd gram)(fst gram)) accept frag);;
					
 (* Make Parser *)

let empty_acceptor suffix =
	match suffix with
		| [] -> Some []
		| _ -> None;;


let rec length rtn_func rule_list accept frag =
	match rule_list with 
		| [] -> None
		| head::tail -> (match height rtn_func head accept frag with
			| None -> length rtn_func tail accept frag
			| Some x -> Some (head::x))
and height rtn_func rule accept frag = 	
	match rule with
		| [] -> accept frag
		| _ -> (match frag with
			| [] -> None
			| head::tail -> (match rule with
				| [] -> None
				| (T term)::rhs -> if head = term then (height rtn_func rhs accept tail) else None
				| (N nterm)::rhs -> (length rtn_func (rtn_func nterm) (height rtn_func rhs accept) frag)));;

let make_traversed_rules gram accept frag = (length (snd gram) ((snd gram)(fst gram)) empty_acceptor frag);;
			
let rec wide start list = 
	match start with 
		| [] -> (list, []) 
		| head::tail -> (match (deep head list) with 
			| (w,x) -> (match wide tail w with 
				| (y,z) -> (y, x::z))) 
and deep start list =
	match start with 
		| (T a) -> (match list with 
			| [] -> ([], Leaf a) 
			| head::tail -> (head::tail, Leaf a)) 
			| (N a) -> (match list with 
				| [] -> ([], Node (a, [])) 
				| head::tail -> (match wide head tail with 
					| (w,x) -> (w, Node (a, x))));; 
							
let traverser gram1 = fun frag -> (make_traversed_rules gram1 empty_acceptor frag);; 

let make_parser gram1 frag = match traverser gram1 frag with 
	| None -> None 
	| Some [] -> None 
	| Some x -> (match wide [N (fst gram1)] x with 
		| (_,y) -> (match y with [] -> None 
			| head::tail -> Some head));;		