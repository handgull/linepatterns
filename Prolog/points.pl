:- load_files(['linepatterns']).
genpoints :-
  ansi_format([bold,fg(green)], 'Creating random points...\n', []),
  randpoints(200, Points),% Choose n random Points
  ansi_format([bold,fg(green)], 'Points correctly generated...\n', []),
  writepoints('points.csv',Points),
  ansi_format([bold,fg(green)], 'Done!', []), !.
randpoints(0, []) :- !.
randpoints(HowMuch, [point(X, Y) | Points]) :-
  random(-10,10, X), random(-10,10, Y), HowMuchM1 is HowMuch - 1,
  randpoints(HowMuchM1, Points), !.
