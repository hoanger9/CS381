% Group members:
%  * Eric Hoang, 933199375
%  * Sun Dongyi, 933966659
%
% Grading notes: 10pts total
%  * Part 1: 6pts (1pt each)
%  * Part 2: 4pts (3pts for cmd, 1pt for prog)


% Part 1. It's a bird-eat-bug world out there!

% A small database of animals. Each relation gives the animal's name,
% it's habitat, and its biological class.
animal(cranefly, trees, insects).
animal(duck, ponds, birds).
animal(minnow, ponds, fish).
animal(scrubjay, trees, birds).
animal(squirrel, trees, mammals).
animal(waterstrider, ponds, insects).
animal(woodpecker, trees, birds).

% A small database capturing what each animal eats. Note that most animals eat
% more than one kind of food, but craneflies don't eat anything after they
% reach adulthood!
diet(scrubjay, insects).
diet(scrubjay, seeds).
diet(squirrel, nuts).
diet(squirrel, seeds).
diet(duck, algae).
diet(duck, fish).
diet(duck, insects).
diet(minnow, algae).
diet(minnow, insects).
diet(waterstrider, insects).
diet(woodpecker, insects).

% A binary predicate that includes all of the animals and where they live.
habitat(Animal, Where) :- animal(Animal, Where, _).

% A binary predicate that includes each animal and its biological class.
class(Animal, Class) :- animal(Animal, _, Class).


% 1. Define a predicate neighbor/2 that determines whether two animals live
%    in the same habitat. Note that two animals of the same kind always
%    live in the same habitat.
neighbor(X, Y) :- habitat(X, W), habitat(Y, W).

% 2. Define a predicate related/2 that includes all pairs of animals that
%    are in the same biological class but are not the same kind of animal.
related(X, Y) :- class(X,Z), class(Y,Z), Y \= X.

% 3. Define a predicate competitor/3 that includes two kinds of animals and
%    the food they compete for. Two animals are competitors if they live in
%    the same place and eat the same food.
competitor(X, Y, Z) :- neighbor(X, Y), diet(X, Z), diet(Y, Z).

% 4. Define a predicate would_eat/2 that includes all pairs of animals where
%    the first animal would eat the second animal (because the second animal
%    is a kind of food it eats), if it could.
would_eat(X, Y) :- diet(X, Z), class(Y, Z).

% 5. Define a predicate does_eat/2 that includes all pairs of animals where
%    the first animal would eat the second, and both animals live in the same
%    place, so it probably does.
does_eat(X, Y) :- neighbor(X, Y), would_eat(X, Y).

% 6. Define a predicate cannibal/1 that includes all animals that might eat
%    their own kind--eek!
cannibal(X) :- would_eat(X, X).


% Part 2. Implementing a stack language

% A slightly larger example program to use in testing.
example(P) :-
  P = [ 2, 3, 4, lte,            % [tru, 2]
        if([5, 6, add], [fls]),  % [11, 2]
        3, swap,                 % [11, 3, 2]
        4, 5, add,               % [9, 11, 3, 2]
        lte, if([tru], [mul]),   % [6]
        "whew!", swap,           % [6, "whew!"]
        "the answer is" ].       % ["the answer is", 6, "whew!"]

% 1. Define the predicate `cmd/3`.
cmd(X, S1, S2)             :- (number(X); string(X); X = tru; X = fls), S2 = [X|S1].
cmd(dup, [H|T], S)         :- S = [H,H|T].
cmd(swap, [N1,N2|T], S)    :- S = [N2,N1|T].
cmd(add, [N1,N2|T], S)     :- X is (N1 + N2), S = [X|T].
cmd(mul, [N1,N2|T], S)     :- X is (N1 * N2), S = [X|T].
cmd(lte, [N1,N2|T], S)     :- (N2 =< N1 -> B = tru; B = fls), S = [B|T].
cmd(if(P1, _), [tru|T], S) :- prog(P1, T, S).
cmd(if(_, P2), [fls|T], S) :- prog(P2, T, S).


% 2. Define the predicate `prog/3`.
prog([], S1, S2)    :- S1 = S2.
prog([C|T], S1, S2) :- cmd(C, S1, S3), prog(T, S3, S2).