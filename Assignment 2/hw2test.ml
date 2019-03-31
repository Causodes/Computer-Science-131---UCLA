let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

type ogre_nonterminals =
  | Talk | Threat | Statement | Question | Shout | Person

let ogre_grammar = 
  (Talk, function
    | Talk -> 		[[N Threat]; [N Statement]; [N Question]; [N Shout]; [N Person]]
    | Threat -> 	[[T"Get out of my swamp, "; N Person]; 
					[T"This is the part where you run away, "; N Person]]
	| Statement ->	[[T"That's a big castle Farquaad has got there."; N Question]; 
					[T"No, but I think you need some compensating of your own!"; N Question]]
    | Question -> 	[[T"Are you Princess Fiona?"]; 
					[T"Gee, you think he's compensating for something?"; N Statement];
					[T"Listen, little donkey. Take a look at me. What am I?"]]
    | Shout -> 		[[T"THIS IS MY SWAMP!"; N Shout]; [T"ROAR!"; N Shout]] 
	| Person -> 	[[T"Donkey"]; [T"Fiona"]; [T"Farquaad"]; [T"Peasant"]])

let small_complete_ogre_frag = ["That's a big castle Farquaad has got there."; "Gee, you think he's compensating for something?"; "No, but I think you need some compensating of your own!"; "Listen, little donkey. Take a look at me. What am I?"]
let small_incomplete_ogre_frag = ["That's a big castle Farquaad has got there."; "Gee, you think he's compensating for something?"; "No, but I think you need some compensating of your own!"; "Listen, little donkey. Take a look at me. What am I?"; "Peasant"]

let test0 = ((make_matcher ogre_grammar accept_all ["ROAR!"; "THIS IS MY SWAMP!"]) = None)
let test1 = ((make_matcher ogre_grammar accept_all ["Donkey"; "Farquaad";]) = Some ["Farquaad"])
let test2 = ((make_matcher ogre_grammar accept_all ["Get out of my swamp, "; "Donkey";]) = Some [])
let test3 =  (make_parser ogre_grammar small_complete_ogre_frag) =
	Some (Node (Talk,
		[Node (Statement,
			[Leaf "That's a big castle Farquaad has got there.";
			Node (Question,
				[Leaf "Gee, you think he's compensating for something?";
				Node (Statement,
					[Leaf "No, but I think you need some compensating of your own!";
					Node (Question,
						[Leaf "Listen, little donkey. Take a look at me. What am I?"])])])])]))						
let test4 =  (make_parser ogre_grammar small_incomplete_ogre_frag) = None