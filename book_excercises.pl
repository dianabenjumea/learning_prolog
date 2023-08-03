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
	


/* In this modified version, we introduce a new predicate significant_better_consumption/2 
to determine if one consumption value is significantly better than the other. 
We calculate a threshold value as half of the average of the two consumptions 
(Threshold is (Con1 + Con2) / 40). Then, we check if the second consumption value (Con2) 
is greater than this threshold plus the first consumption value (Con1 > GoodThreshold). 
If this condition is met, we consider the second consumption as significantly better. */

/* Now, the sometimes_better predicate will prefer Car2 over Car1 if Car2 has at least one 
test result that is significantly better than the corresponding test result of Car1. 
If neither car is significantly better in any test, the predicate falls back to using 
the equal_or_better_consumption definition to determine preference. */

/* With this modification, the program will still compare all the test results for each car,
 but the preference will be based on significant differences, not just on whether one car
  is always better than the other. */


%Sometimes_better definition

significant_better_consumption(Con1, Con2) :-
    Threshold is (Con1 + Con2) / 40,
    GoodThreshold is Con1 + Threshold,
    Con2 > GoodThreshold.

sometimes_better([Con1|_], [Con2|_]) :-
    significant_better_consumption(Con1, Con2).

sometimes_better([_|Con1], [_|Con2]) :-
    sometimes_better(Con1, Con2).


%CHAPTER 4
father(mary, george).
father(john, george).
father(sue, harry).
father(george, edward).

child(X, Y) :- father(Y, X).
father(X) :- father)(_, X).

/* Adam is a operson, anything is a person if it has a mother, and eve is a person.
Also, various people have various mothers */

person(adam).
person(X) :- mother(X,Y).
person(eve).

mother(cain, eve).
mother(abel, eve).
mother(jabal, adah).
mother(tubalcain, zillah).


/*  Partie example who migh dance with whom */
possible_pair(X,Y) :- boy(X), girl(Y).

boy(john).
boy(marmaduke).
boy(bertram).
boy(charles).

girl(griselda).
girl(ermintrude).
girl(brunhilde).

/* INTEGER DEFINITION */
is_integer(0).
is_integer(X) :- is_integer(Y), X is Y+1.



/* CUT */
favility(Pers, fac) :-
	book_overdue(Pers,Book),
	!,
	basic_facility(Fac)
facility(Per, Fac) :- general_facility(Fac).

basic_facility(reference).
basic_facility(enquiries).

aditional_facility(borrowing).
aditional_facility(inter_library_loan).

general_facility(X) :- basic_facility(X).
general_facility(X) :- aditional_facility(X).

%Clientes database
client('A. Jones').
client('W. Metesk').

book_overdue('C. Watzer', book10089).
book_overdue('A. Jones', book29907).












