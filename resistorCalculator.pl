% Make dynamic nodes,resistor,value
:- dynamic(node/1).
:- dynamic(resistor/1).
:- dynamic(connected/2).
:- dynamic(value/2).


demo1() :-
    retractall(value(_,_)),
    retractall(node(_)),
    retractall(resistor(_)),
    retractall(connected(_,_)),
    assertz(node(n1)),
    assertz(node(n2)),
    assertz(node(n3)),
    assertz(node(n4)),
    assertz(resistor(r1)),
    assertz(resistor(r2)),
    assertz(resistor(r3)),
    assertz(resistor(r4)),
    assertz(value(r1,100)),
    assertz(value(r2,200)),
    assertz(value(r3,100)),
    assertz(value(r4,300)),
    assertz(connected(r1,n1)),
    assertz(connected(r1,n2)),
    assertz(connected(r3,n1)),
    assertz(connected(r3,n2)),
    assertz(connected(r2,n2)),
    assertz(connected(r2,n3)),
    assertz(connected(r4,n3)),
    assertz(connected(r4,n4)),
    writeln("Initialized").


sequential(X,Y) :-
    dif(X,Y),
    connected(X,N),
    node(N),
    connected(X,N1),
    node(N1),
    dif(N,N1),
    connected(Y,N),
    not(connected(Y,N1)),
    not(parallel(X,_));
    (dif(Y,X),
    connected(Y,N),
    node(Y),
    connected(Y,N1),
    node(N1),
    dif(N,N1),
    connected(X,N),
    not(connected(X,N1)),
    not(parallel(Y,_))).
    


parallel(X,Y) :-
    dif(X,Y),
    connected(X,N),
    node(N),
    connected(X,N1),
    node(N1),
    dif(N,N1),
    connected(Y,N),
    connected(Y,N1).


calculate_serie_resistance(X,Y,Z) :- Z is (X+Y).
calculate_parallel_resistance(X,Y,Z) :- Z is ((X*Y) / (X+Y)).

solve_parallel(X,Y) :-
    parallel(X,Y),
    value(X,V1),
    value(Y,V2),
    calculate_parallel_resistance(V1,V2,R),
    retract(resistor(Y)),
    retract(connected(Y,_)),
    retract(value(Y,_)),
    retract(value(X,_)),
    assertz(value(X,R)).


solve_sequential(X,Y) :-
    sequential(X,Y),
    value(X,V1),
    value(Y,V2),
    connected(X,N1),
    connected(Y,N1),
    calculate_serie_resistance(V1,V2,R),
    retract(resistor(Y)),
    retract(value(Y,_)),
    retract(value(X,_)),
    assertz(value(X,R)),
    unique_node(X,Y,Un1),
    unique_node(Y,X,Un2),
    assertz(connected(X,Un1)),
    assertz(connected(Un1,X)),
    assertz(connected(Un2,X)),
    assertz(connected(X,Un2)).

unique_node(C1,C2,Y) :-
    connected(C1,Y),
    not(connected(C2,Y)).

solve_all_parallel(X,Y) :-
    resistor(X),
    resistor(Y),
    solve_parallel(X,Y),
    solve_all_parallel(X,Y).

solve_all_sequential(X,Y) :-
    resistor(X),
    resistor(Y),
    solve_sequential(X,Y),
    solve_all_sequential(X,Y).

solve(X,Y) :-
    (solve_all_parallel(X,Y));
    (solve_all_sequential(X,Y)),
    test(X,Y).

solve(X,Y) :-
    findall(X,resistor(X),L),
    length(L,Y),
    Y == 1.

showResult(X,Y) :-
    findall(X,resistor(X),L),
    length(L,Y),
    Y == 1,
    resistor(X),
    writeln(X),
    value(X,Z),
    writeln(Z).