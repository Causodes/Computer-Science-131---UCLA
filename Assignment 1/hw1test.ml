let my_subset_test0 = subset [] []
let my_subset_test1 = subset [5;5;5] [5]
let my_subset_test2 = not (subset [2;4] [4;1;3])

let my_equal_sets_test0 = equal_sets [1;3] [3;1;3]
let my_equal_sets_test1 = equal_sets [] []
let my_equal_sets_test2 = not (equal_sets [1;4;4] [4;1;3])

let my_set_union_test0 = equal_sets (set_union [] [2;2;3]) [2;3]
let my_set_union_test1 = equal_sets (set_union [1;2;3] [1;2;3]) [1;2;3]
let my_set_union_test2 = equal_sets (set_union [2] []) [2]

let my_set_intersection_test0 =
  equal_sets (set_intersection [] []) []
let my_set_intersection_test1 =
  equal_sets (set_intersection [3;5] [1;2;3]) [3]
let my_set_intersection_test2 =
  equal_sets (set_intersection [0;0;0] [3;1;2;4]) []

let my_set_diff_test0 = equal_sets (set_diff [0] [0;1]) []
let my_set_diff_test1 = equal_sets (set_diff [1;2;3;4] [4;5;6;7]) [1;2;3]
let my_set_diff_test2 = equal_sets (set_diff [6;2;6] []) [2;6]
let my_set_diff_test3 = equal_sets (set_diff [] [0]) []

let my_computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x / 3) 999 = 0
let my_computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x *. 4.) 5. = infinity
let my_computed_fixed_point_test2 =
  computed_fixed_point (=) sqrt 16. = 1.

type my_filter_reachable_nonterminals_0 =
  | Subject | Sentence | Space | Adjective | Punctuation

let test0_grammar =
  Subject,
  [Adjective, [T"red"];
   Space, [T" "];
   Punctuation, [T"."];
   Sentence, [N Adjective];
   Sentence, [N Space];
   Sentence, [N Punctuation];
   Subject, [N Adjective];
   Subject, [N Sentence; T","; N Subject]]

let my_filter_reachable_test0 =
  filter_reachable test0_grammar = test0_grammar