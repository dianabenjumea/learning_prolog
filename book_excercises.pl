dog(blacky).
cat(tom).
mice(jerry).


%EXERCISE 3.1
%Using the sometimes_better definition, almost every car will be preferred 
%to each other. Change the program so that it prefers car to Another if at 
%least one of the test resuls is significant better (where you will have to 
%decide what you mean by "significant")


fuel_consumed(waster, [3.1, 10.4, 15.9, 10.3]).
fuel_consumed(guzzler, [3.2, 9.9, 13.0, 11.6]).
fuel_consumed(prodigal, [2.8, 9.8, 13.1, 10.4]).


equal_or_better_consumption(Good, Bad) :-
	Threshold is (Good + Bad) / 40,
	Worst is Bad + Threshold,
	Good < Worst.


prefer(Car1, Car2) :-
	fuel_consumed(Car1, Con1),
	fuel_consumed(Car2, Con2),
	always_better(Con1, Con2).


always_better([], []).
always_better([Cond1|T1], [Con2|T2]) :-
	equal_or_better_consumption(Con1, Con2),
	always_better(T1, T2).


%sometimes_better definition

%sometimes_better([Con1|_], [Con2|_]) :-
%	equal_or_better_consumption(Con1, Con2).

%sometimes_better([_|Con1], [_|Con2]) :-
%	sometimes_better(Con1, Con2)
	


%New sometimes_better definition

sometimes_better([Con1|_], [Con2|_]) :-
	equal_or_better_consumption(Con1, Con2).

sometimes_better([_|Con1], [_|Con2]) :-
	sometimes_better(Con1, Con2).


%CHAPTER 4
father(mary, george).
father(john, george).
father(sue, harry).
father(george, edward).

child(X, Y) :- father(Y, X).
father(X) :- father)(_, X).













