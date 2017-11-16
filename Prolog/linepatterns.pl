%%% 806752 Andrea Gullì
%%% isapoint(Point).
% Dove Point è un punto avente coordinate X e Y.
% @area2/4, angle2d/3, isaslope/1, isasegment/1, readpoints/2, writepoints/2
isapoint(point(X, Y)) :- number(X), number(Y), !.
%%% isasegment(Segment).
% Dove Segment è un segmento così formato: line([point(X, Y), ...]).
% @write_segments/2
isasegment(line(Points)) :- foreach(member(P, Points), isapoint(P)), !.
%%% area2(A, B, C, Area).
% Dove A, B e C sono dei punti e Area è il doppio dell'area del triangolo ABC
% @left/3, left_on/3, collinear/3
area2(A, B, C, Area) :- isapoint(A), isapoint(B), isapoint(C),
  A=..[point, XA, YA], B=..[point, XB, YB], C=..[point, XC, YC],
  Area is ((XB - XA) * (YC - YA)) - ((YB - YA) * (XC - XA)), !.
%%% left(A, B, C).
% Il predicato ritorna true se l'angolo ABC È un angolo a sinistra
left(A, B, C) :- area2(A, B, C, Area), Area > 0, !.
%%% left_on(A, B, C).
% Il predicato ritorna true se l'angolo ABC È un angolo a destra
left_on(A, B, C) :- area2(A, B, C, Area), Area < 0, !.
%%% collinear(A, B, C).
% Il predicato ritorna true se i punti A, B e C sono collineari
collinear(A, B, C) :- area2(A, B, C, Area), Area is 0, !.
%%% angle2d(A, B, R).
% Dove A e B sono due punti ed R è l'angolo in radianti tra A e B
% @foreach_angle2d/3
angle2d(A, B, R) :- isapoint(A), isapoint(B),
  A=..[point, XA, YA], B=..[point, XB, YB],
  atan2(XA - XB, YA - YB, R), !.
%%% line_patterns(Points, Lines). PREDICATO PRINCIPALE
% Dove Points è una lista di punti e Lines è una lista di segmenti
% da almeno 4 punti collineari
line_patterns([], []) :- !.% Caso base
line_patterns(Points, Lines) :-
  length(Points, CountDown),
  rmduplicates(Points, Ps), get_segments(CountDown, Ps, Segments),
  flatten(Segments, FSegments),
  get_lines(FSegments, DoubleLines),
  msort(DoubleLines, OLines), rmduplicates(OLines, Lines), !.
%%% rmduplicates(List, ShrinkedList).
% Dove ShrinkedList è List rimuovendo gli elementi duplicati
% @line_patterns/2
rmduplicates([], []) :- !.% Caso base
% X compare anche nella coda (lo verifico con la member) lo tolgo e ricomincio
rmduplicates([X | Xs], Y) :- member(X, Xs), rmduplicates(Xs,Y), !.
% X non compare anche nella coda, allora tolgo un elemento ad entrambe le code
rmduplicates([X | Xs], [X | Ys]) :- rmduplicates(Xs,Ys), !.
%%% get_segments(CountDown, Points, Lines).
% Dove Points è una lista di punti e Lines è una lista di segmenti
% PS Ogni segmento sarà presente 2 volte!
% @line_patterns/2
get_segments(0, _, []) :- !.% Caso base
get_segments(CountDown, [P | Points], [SomeLines | Lines]) :-
  foreach_angle2d(P, Points, Slopes),
  find_segments(P, Slopes, SomeLines),
  CM1 is CountDown - 1,
  flatten([Points | P], RPoints),
  get_segments(CM1, RPoints, Lines), !.
%%% foreach_angle2d(Point, OthersPoints, SlopesByPoint).
% Chiama angle2d per ogni elemento in OthersPoints
% @get_segments/2
foreach_angle2d(_, [], []) :- !.% Caso base
foreach_angle2d(Point, [P | Ps], [slope(Angle, P) | SlopesByP]) :-
  angle2d(Point, P, Angle),
  foreach_angle2d(Point, Ps, SlopesByP), !.
%%% find_segments(Pivot, OSlopes, Lines).
% Dove OSlopes è una lista ordinata di slope(_, _, _) e Lines è l'insieme di
% tutti i segmenti ottenibili.
% e pivot è il punto su cui si basano gli slopes.
find_segments(_, [], []):- !.% Caso base
% Caso in cui trovo una linea usando gli slope simili a Slope
find_segments(Pivot, [Slope | Slopes], [line([Pivot | Line]) | Lines]) :-
  mksubl(Slope, Slopes, Line, Others),
  find_segments(Pivot, Others, Lines), !.
% Caso in cui non ho abbastanza slope simili a Slope
find_segments(Pivot, [Slope | Slopes], Lines) :-
  mksubl(Slope, Slopes, [], Others),
  find_segments(Pivot, Others, Lines), !.
%%% mksubl(Slope, Slopes, Line, OtherSlopes).
% Dove Slope è lo slope che prendo in considerazione per trovarne di simili
% Slopes è la lista contenente gli altri slopes
% Line è una lista di punti
% OtherSlopes contiene gli slope che avevano un angolo diverso
% @find_segments/3
mksubl(slope(_, P), [], [P], []) :- !.% Caso base
% Slope e il primo Slope in Slopes sono simili
  mksubl(Slope1, [Slope2 | Slopes], [P2 | Points], Lines):-
  Slope1=..[slope, A, _], Slope2=..[slope, A, P2],
  mksubl(Slope1, Slopes, Points, Lines), !.
% Slope e il primo Slope in Slopes non sono simili
mksubl(Slope1, [Slope2 | Slopes], Points, [Slope2 | Others]) :-
  Slope1=..[slope, A1, _], Slope2=..[slope, A2, _],
  A1 \= A2,
  mksubl(Slope1, Slopes, Points, Others), !.
%%% get_lines(Segments, Lines).
% Dove segments è un insieme di linee composte da 2 o più punti
% e Lines è l'insieme di linee composte da 4 o più punti
% @line_patterns/2
get_lines([], []) :- !.% Caso base
get_lines([line(Points) | Segms], [line(OPoints) | Lines]) :-
  length(Points, X), X >= 4, msort(Points, OPoints), get_lines(Segms, Lines), !.
get_lines([line(Points) | Segms], Lines) :-
  length(Points, X), X < 4, get_lines(Segms, Lines),!.
%%% readpoints(Filename, Points).
% Legge un insieme di punti da un file
readpoints(Filename, Points) :-
  (csv_read_file(
     Filename, Points, [separator(0'\t), functor(point), arity(2)]);
   csv_read_file(
     Filename, Points, [separator(0'\s), functor(point), arity(2)])),
  foreach(member(P, Points), isapoint(P)),
  !.
%%% writepoints(Filename, Points).
% Scrive un insieme di punti su un file
writepoints(Filename, Points) :-
  csv_write_file(Filename, Points, [separator(0'\t), functor(point), arity(2)]),
  foreach(member(P, Points), isapoint(P)), !.
%%% write_segments(Filename, Segments).
% Scrive un insieme di segmenti su un file
write_segments(Filename, Segments) :-
  foreach(member(S, Segments), isasegment(S)),
  smallparser(Segments, PSegments),
  create_functors(PSegments, Terms),
  csv_write_file(Filename, Terms, [separator(0'\t), functor(line)]), !.
%%% smallparser(Segments, ParsedSegments).
% Dove ParsedSegments è Segments parsato
% in modo tale da essere facilmente scrivibile da
% @write_segments/2
smallparser([], []) :- !.% Caso base.
smallparser([line([point(X, Y) | Ps]) | Ls], [[X, Y | PPs] | PLs]) :-
  smallparser([line(Ps) | Ls], [PPs | PLs]), !.
smallparser([line([]) | Ls], [[] | PLs]) :-
  smallparser(Ls, PLs), !.
%%% create_functors(Lists, Terms).
% Data una lista di liste Lists crea una lista di predicati line con n parametri
% @write_segments/2
create_functors([], []) :- !.% Caso base
create_functors([Line | Lines], [Term | PLines]) :-
  Term =.. [line | Line], create_functors(Lines, PLines), !.
