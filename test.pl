append([],L,L).
append([X|Xs],L,[X|L2]) :- append(Xs,L,L2).
