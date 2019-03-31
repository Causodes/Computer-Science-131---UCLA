tower(N, T, C) :-
	length(T, N),
	C = counts(TOPROW, BOTROW, LEFTCOL, RIGHTCOL),
	length(LEFTCOL, N),
	length(RIGHTCOL, N),
	length(TOPROW, N),
	length(BOTROW, N),
	maplist(flipLength(N), T),
	maplist(domain(N), T),
	maplist(fd_all_different, T),
	transpose(T, T2),
	maplist(fd_all_different, T2),
	check(T, LEFTCOL, N),
	recheck(T, RIGHTCOL, N),
	check(T2, TOPROW, N),
	recheck(T2, BOTROW, N),
	maplist(fd_labeling, T).
	
%plain_tower(N, T, C) :-
%	length(T, N),
%	C = counts(TOPROW, BOTROW, LEFTCOL, RIGHTCOL),
%	length(LEFTCOL, N),
%	length(RIGHTCOL, N),
%	length(TOPROW, N),
%	length(BOTROW, N),
%	maplist(flipLength(N), T),
%	maplist(domain_custom(N), T),
%	maplist(custom_all_different, T),
%	transpose(T, T2),
%	maplist(custom_all_different, T2),
%	custom_check(T, LEFTCOL, N),
%	custom_recheck(T, RIGHTCOL, N),
%	custom_check(T2, TOPROW, N),
%	custom_recheck(T2, BOTROW, N),
%	maplist(custom_labeling(N), T).

plain_tower(N, T, C) :- 
	length(T, N),
	C = counts(TOPROW, BOTROW, LEFTCOL, RIGHTCOL),
	length(LEFTCOL, N),
	length(RIGHTCOL, N),
	length(TOPROW, N),
	length(BOTROW, N),
	plain_matrix(N, T, LEFTCOL, RIGHTCOL),
	transpose(T, T2),
	plain_matrix(N, T2, TOPROW, BOTROW).

plain_matrix(N, [], _, _).
plain_matrix(N, [THead | TBody], [LeftHead | LeftBody], [RightHead | RightBody]) :- 
	length(NList, N),
	make_NList(N, NList), !,
	permutation(NList, THead),
	member(LeftHead, NList),
	plain_count(LeftHead, THead, 0),
	reverse(THead, THeadRev),
	member(RightHead, NList),
	plain_count(RightHead, THeadRev, 0),
	plain_matrix(N, TBody, LeftBody, RightBody).
	
make_NList(0, []).
make_NList(N, [Head | Tail]) :-
	NewN is N-1,
	Head is N,
	make_NList(NewN, Tail).

plain_count(1, [T], Height) :-
	T > Height.

plain_count(0, [T], Height) :-
	T < Height.

plain_count(Count, [Head | Tail], Height) :-
	Head > Height, 
	NewCount is Count - 1,
	goodcount2(NewCount, Tail, Head).

plain_count(Count, [Head | Tail], Height) :-
	Head < Height, 
	goodcount2(Count, Tail, Height).

flipLength(Size, Object) :-
	length(Object, Size).
	
domain(N, []).
domain(N, [Head | Tail]) :-
	fd_domain(Head, 1, N),
	domain(N, Tail).
	
check([], _, N).
check(T, CountList, N) :-
	T = [Head | Tail],
	CountList = [First | Body],	
	get_count(Head, Count, 0),
	First is Count,
	check(Tail, Body, N).
	
recheck([], _, N).
recheck(T, CountList, N) :-
	T = [Head | Tail],
	reverse(Head, FlippedHead),
	CountList = [First | Body],
	get_count(FlippedHead, Count, 0),
	First = Count,
	recheck(Tail, Body, N).
	
get_count([], 0, _).
get_count([First | Body], Count, Tallest) :-
	First #< Tallest,
	get_count(Body, Count, Tallest);
	First #> Tallest,
	NewCount #= Count - 1,
	get_count(Body, NewCount, First).
	
%custom_check([], _, N).
%custom_check(T, CountList, N) :-
%	T = [Head | Tail],
%	CountList = [First | Body],	
%	member(Count, [1, 2, 3, 4]),
%	custom_get_count(Head, Count, 0),
%	First is Count,
%	custom_check(Tail, Body, N).
	
%custom_recheck([], _, N).
%custom_recheck(T, CountList, N) :-
%	T = [Head | Tail],
%	reverse(Head, FlippedHead),
%	CountList = [First | Body],
%	member(Count, [1, 2, 3, 4]),
%	custom_get_count(FlippedHead, Count, 0),
%	First = Count,
%	custom_recheck(Tail, Body, N).
	
%custom_get_count([], 0, _).
%custom_get_count([First | Body], Count, Tallest) :-
%	member(First, [1, 2, 3, 4]),
%	First < Tallest,
%	custom_get_count(Body, Count, Tallest);
%	First > Tallest,
%	NewCount is Count - 1,
%	custom_get_count(Body, NewCount, First).
	
%custom_domain([], _, _).
%custom_domain([Head | Tail], Minimum, Maximum) :-
%	member(Head, [1, 2, 3, 4]),
%	Head > Minimum - 1,
%	Head < Maximum + 1,
%	custom_domain(Tail, Minimum, Maximum).
	
%domain_custom(N, []).
%domain_custom(N, T) :-
%	custom_domain(T, 1, N).
	
%element_not_in_list(_, []).
%element_not_in_list(Element, [Head | Tail]) :-
%	member(Head, [1, 2, 3, 4]),
%	member(Element, [1, 2, 3, 4]),
%	Element \= Head,
%	element_not_in_list(Element, Tail).
	
%custom_all_different([]).
%custom_all_different([Head | Tail]) :-
%	element_not_in_list(Head, Tail),
%	custom_all_different(Tail).
	
%custom_labeling(N, L) :-
%	findall(Num, between(1, N, Num), X), 
%	permutation(X, L).
	
% transpose copied pasted from SWI prolog

transpose([], []).
transpose([F|Fs], Ts) :-
	transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
	lists_firsts_rests(Ms, Ts, Ms1),
	transpose(Rs, Ms1, Tss).
	
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
	lists_firsts_rests(Rest, Fs, Oss).
	
tower_time(TimeTaken) :-
	statistics(runtime, [T1|_]),
	tower(5, T, counts([2, 1, 4, 3, 2], [2, 3, 1, 3, 2], [2, 1, 4, 4, 2], [2, 4, 2, 1, 2])),
	statistics(runtime, [T2|_]),
	TimeTaken is T2 - T1.

plain_tower_time(TimeTaken) :-
	statistics(runtime, [T1|_]),
	plain_tower(5, T, counts([2, 1, 4, 3, 2], [2, 3, 1, 3, 2], [2, 1, 4, 4, 2], [2, 4, 2, 1, 2])),
	statistics(runtime, [T2|_]),
	TimeTaken is T2 - T1.

speedup_ratio(R) :-
	tower_time(T1),
	plain_tower_time(T2),
	R is T2 / T1.