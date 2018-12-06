%Cave Definition
cave([[0, 0, 2, 3], [2, 0, 1, 2], [3, 0, 3, 1], [6, 0, 1, 2], [7, 0, 1, 3], [8, 0, 4, 4], [9, 4, 3, 1], [10, 5, 2, 1], [0, 4, 6, 6], [3, 3, 3, 1], [4, 2, 1, 1], [6, 5, 1, 5], [7, 8, 1, 2], [8, 9, 3, 1], [11, 8, 1, 2]]).

%Base Case.
insideBounds([],[_,_],_) :- !.

%Compares cave boundaries and endpoint against test point X,Y
%Returns true if test point is safe
insideBounds([Head|Tail],[X, Y],CaveWidth) :-
    Head = [RectX, RectY, Width, Height],
    RectWidth is Width + RectX,
    RectHeight is Height + RectY,
    boundsCheck(RectX,RectY,RectWidth,RectHeight,X,Y),
    !,%Cut is used to prevent backtracking to ;'s in boundsCheck
    X @< CaveWidth,
    insideBounds(Tail, [X,Y],CaveWidth).

%Sub rule used in insideBounds to check points
boundsCheck(RectX, RectY, RectWidth, RectHeight, X, Y) :-
    X @< RectX; X @>= RectWidth; Y @< RectY; Y @>= RectHeight.

%Chooses offset with bias on increasing x coord
%Calculates and returns absolute coordinate
move([PrevX,PrevY],NextCoord) :-
    Offset = [TempX,TempY],
    member(Offset,[[1,0],[0,1],[0,-1]]),
    X is TempX + PrevX,
    Y is TempY + PrevY,
    NextCoord = [X,Y].

%Base case
not_member(_, []) :- !.

%Class code
not_member(X, [Head|Tail]) :-
    X \= Head,
    not_member(X, Tail).

%Base case, checks if x coord matchs width of cave
fly(CaveWidth, Path) :-
    Path = [Head|_],
    Head = [X, _],
    X is CaveWidth - 1,
    print(Path).

%Picks direction to move
%Checks if position was to moved to before
%Checks if position in inside boundaries
%Then recurses until base case is met
%Need to pass the starting location as the first element of the path
fly(CaveWidth, Path) :-
    Path = [[PrevX,PrevY]|Tail],
    cave(X),
    move([PrevX,PrevY], NextCoord),
    not_member(NextCoord,Path),
    insideBounds(X, NextCoord, CaveWidth),
    fly(CaveWidth, [NextCoord,[PrevX,PrevY]|Tail]).
























































