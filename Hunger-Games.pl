 /* Dynamic Variables */
:- dynamic(current_Health/1).
:- dynamic(current_Hunger/1).
:- dynamic(current_Thirst/1).
:- dynamic(at/3).
:- dynamic(damage/1).
:- dynamic(i_am_at/2).
:- dynamic(nw/2).
:- dynamic(n/2).
:- dynamic(nea/2).
:- dynamic(we/2).
:- dynamic(e/2).
:- dynamic(sw/2).
:- dynamic(se/2).
:- dynamic(so/2).
:- dynamic(matrix/1).
:- dynamic(inventory/1).
:- dynamic(hands/1).
:- dynamic(mapWeapon/3).
:- dynamic(mapFood/3).
:- dynamic(mapMedicine/3).
:- dynamic(mapEnemy/2).
:- dynamic(mapRadar/3).
:- dynamic(enemyCount/1).

/* Fact */
i_am_at(4, 11).
inventory([water_pouch]).

/* adjacent(R,C) */
nw(3,10). /* barat laut */
n(3,11). /* utara */
nea(3,12). /* timur laut */
we(4,10). /* barat */
e(4,12). /* timur */
sw(5,10). /* barat daya */
se(5,12). /* tenggara */
so(5,11). /* selatan */

/* Item List */
/* Weapon name(weapon, X, Y, use)*/
weapon(rock).
weapon(knife).
weapon(axe).
weapon(bow).
weapon(spear).
weapon(pistol).
weapon(mjolnir).
weapon(bomb).

radar(radar).

food(blackberries, 15).
food(eggs, 10).
food(fish, 15).
food(apple, 20).
food(bread, 45).
food(canned_soup, 50).
food(meat, 60).
food(soup, 30).
food(geprek_bensu, 80).

medicine(panadol, 15).
medicine(bandage, 10).
medicine(morphling, 40).
medicine(antidote, 50).
medicine(hug, 100).


hands(none).

damage(18).

mapEnemy(4,12). 
mapEnemy(2,4). 
mapEnemy(3,19). 
mapEnemy(4,16). 
mapEnemy(3,10). 
mapEnemy(5,17). 
mapEnemy(6,9). 
mapEnemy(6,15). 
mapEnemy(7,20). 
mapEnemy(8,6). 

enemyCount(10).

mapFood(1,5,soup). 
mapFood(1,10,geprek_bensu). 
mapFood(2,13,bread). 
mapFood(3,6,soup). 
mapFood(3,9,meat). 
mapFood(4,8,meat). 
mapFood(3,16,meat). 
mapFood(5,8,apple). 
mapFood(5,12,soup). 
mapFood(6,4,bread). 
mapFood(6,12,bread). 
mapFood(6,17,soup). 
mapFood(7,2,bread). 
mapFood(8,15,eggs). 
mapFood(8,18,meat). 
mapFood(8,20,meat). 
mapFood(9,1,blackberries). 
mapFood(9,8,fish). 
mapFood(9,10,fish). 
mapFood(10,4,eggs). 
mapFood(10,13,blackberries). 
mapFood(11,17,bread). 
mapFood(11,19,bread). 
mapFood(12,5,geprek_bensu). 
mapFood(12,10,meat). 
mapFood(14,5,soup). 
mapFood(14,7,soup). 
mapFood(15,14,bread). 

mapMedicine(1,3,panadol). 
mapMedicine(4,18,bandage). 
mapMedicine(2,16,morphling).
mapMedicine(5,16,morphling). 
mapMedicine(5,11,bandage). 
mapMedicine(6,18,hug). 
mapMedicine(6,20,morphling). 
mapMedicine(7,11,panadol).
mapMedicine(7,16,morphling). 
mapMedicine(8,6,bandage). 
mapMedicine(8,17,morphling).
mapMedicine(8,18,morphling). 
mapMedicine(9,4,bandage). 
mapMedicine(9,18,panadol). 
mapMedicine(10,9,morphling). 
mapMedicine(11,3,bandage). 
mapMedicine(12,16,hug). 
mapMedicine(13,16,morphling).
mapMedicine(11,16,antidote). 
mapMedicine(14,5,bandage).
mapMedicine(14,16,morphling). 
mapMedicine(15,7,panadol). 
mapMedicine(15,9,antidote).

mapRadar(4,10,radar).

mapWeapon(2,8,rock). 
mapWeapon(2,12,knife). 
mapWeapon(2,14,axe). 
mapWeapon(3,11,spear). 
mapWeapon(4,11,bow). 
mapWeapon(3,17,mjolnir). 
mapWeapon(5,5,bomb). 
mapWeapon(5,13,pistol). 
mapWeapon(6,20,bow). 
mapWeapon(7,9,axe).  
mapWeapon(7,14,rock). 
mapWeapon(7,16,knife). 
mapWeapon(7,18,bomb). 
mapWeapon(8,3,bow). 
mapWeapon(8,12,rock). 

current_Health(100).
current_Hunger(100).
current_Thirst(100).

water(6,7).
water(6,8).
water(8,5).
water(8,6).
water(8,7).
water(8,10).
water(9,8).
water(9,10).
water(10,9).
water(10,10).

/* Rules */
start :-
  write('Welcome to the 77th Hunger Games! You have been chosen as one of the lucky contestants. Be the last man standing and you will be remembered as one of the victors.'), nl,
  write('Available commands:'), nl,
  write('start.          -- start the game!'), nl,
  write('help.           -- show available commands'), nl,
  write('quit.           -- quit the game'), nl,
  write('look.           -- look around you'), nl,
  write('a. s. w. d.     -- move'), nl,
  write('map.            -- look at the map and detect enemies (need radar to use)'), nl,
  write('take(Object).   -- pick up an object drop(Object).   -- drop an object'), nl,
  write('use(Object).    -- use an object'), nl,
  write('attack.         -- attack enemy that crosses your path'), nl,
  write('status.         -- show your status'), nl,
  write('save(Filename). -- save your game'), nl,
  write('load(Filename). -- load previously saved game'), nl,
  write('Legends:'), nl,
  write('M = medicine'), nl,
  write('F = food'), nl,
  write('W = weapon'), nl,
  write('P = player'), nl,
  write('E = enemy'), nl,
  write('- = accessible'), nl,
  write('X = inaccessible'), nl,
  write('Happy Hunger Games! And may the odds be ever in your favor.'), nl,
  i_am_at(X,Y),
  printtempat(X,Y), !,
  printBarang(X,Y), !.


help :-
  write('Available commands:'), nl,
  write('start.          -- start the game!'), nl,
  write('help.           -- show available commands'), nl,
  write('quit.           -- quit the game'), nl,
  write('look.           -- look around you'), nl,
  write('a. s. w. d.     -- move'), nl,
  write('map.            -- look at the map and detect enemies (need radar to use)'), nl,
  write('take(Object).   -- pick up an object drop(Object).   -- drop an object'), nl,
  write('use(Object).    -- use an object'), nl,
  write('attack.         -- attack enemy that crosses your path'), nl,
  write('status.         -- show your status'), nl,
  write('save(Filename). -- save your game'), nl,
  write('load(Filename). -- load previously saved game'), nl,
  write('Legends:'), nl,
  write('M = medicine'), nl,
  write('F = food'), nl,
  write('W = weapon'), nl,
  write('P = player'), nl,
  write('E = enemy'), nl,
  write('- = accessible'), nl,
  write('X = inaccessible'), nl.


quit :- halt.

show_inventory([]) :-
  write('none'), nl.

show_inventory([X]) :-
  write(X), nl.

show_inventory(X) :- 
 X = [A|B], 
 write(A),
 write(', '), 
 show_inventory(B).

status :-  
 write('>Health: '), current_Health(Health), write(Health), !, nl, 
 write('>Hunger: '), current_Hunger(Hunger), write(Hunger), !, nl, 
 write('>Thirst: '), current_Thirst(Thirst), write(Thirst), !, nl, 
 write('>Weapon: '), hands(Weapon), write(Weapon), !, nl, 
 write('>Inventory:'), inventory(List), show_inventory(List), !.

matrix([ 
  ['X','X','X','X','X','X','X','X','X','X','X','X','X','X','X','X','X','X','X','X','X','X'], 
  ['X','X','C','C','C','C','C','G','G','G','G','G','G','J','J','J','J','J','J','J','J','X'], 
  ['X','X','X','C','C','C','C','C','C','G','G','G','G','J','J','J','J','J','J','J','J','X'], 
  ['X','X','X','X','C','C','C','C','C','G','G','G','G','J','X','J','J','J','J','J','J','X'], 
  ['X','X','X','X','C','C','C','G','G','G','G','G','G','X','X','X','J','J','J','J','J','X'], 
  ['X','X','X','C','C','C','C','G','G','G','G','G','G','J','X','J','J','J','J','J','J','X'], 
  ['X','X','C','C','C','X','X','L','L','G','G','G','G','J','J','J','J','J','J','J','J','X'], 
  ['X','X','C','C','C','X','X','X','X','G','G','G','G','J','J','J','J','J','J','J','J','X'], 
  ['X','X','G','G','X','L','L','L','X','X','L','J','J','J','J','J','G','G','G','G','G','X'], 
  ['X','X','G','G','G','G','G','G','L','X','L','J','J','J','J','J','G','G','G','G','G','X'], 
  ['X','G','G','G','G','G','G','G','G','L','L','J','J','J','J','J','G','D','D','D','D','X'], 
  ['X','G','G','G','G','G','G','X','X','X','X','X','X','G','G','G','G','D','D','D','D','X'], 
  ['X','G','X','X','G','G','G','C','C','C','C','C','C','C','X','G','G','D','D','D','D','X'], 
  ['X','X','X','X','G','G','G','X','X','X','X','X','X','C','X','G','G','D','D','D','D','X'], 
  ['X','X','X','X','M','M','M','M','M','M','M','M','M','C','C','X','X','X','X','X','D','X'], 
  ['X','X','X','M','M','M','M','M','M','X','X','M','M','C','C','D','D','D','D','X','X','X'], 
  ['X','X','X','X','X','X','X','X','X','X','X','X','X','X','X','X','X','X','X','X','X','X'] 
]).



printMatrix(M) :- printColumn(M, 0).
printColumn([], _).
printColumn([H|T], R) :-
  printRow(H, R, 0),
  Rpp is R + 1,
  printColumn(T, Rpp).

printRow([], _, _).
printRow([H|T], R, C) :-
  (
   ((nw(R,C); n(R,C); we(R,C); sw(R,C); so(R,C)), ((H = 'X',write(H));
      (mapEnemy(R,C), write('E'));
      (mapRadar(R,C,_), write('R'));
      (mapMedicine(R,C,_), write('M'));
      (mapFood(R,C,_), write('F'));
      (mapWeapon(R,C,_), write('W'));
      (write('-'))
      ), write(' '));
   ((nea(R,C); e(R,C); se(R,C)), ((H = 'X',write(H));
      (mapEnemy(R,C), write('E'));
      (mapRadar(R,C,_), write('R'));
      (mapMedicine(R,C,_), write('M'));
      (mapFood(R,C,_), write('F'));
      (mapWeapon(R,C,_), write('W'));
      (write('-'))
      ), nl);
   (i_am_at(R,C), 
    ((mapEnemy(R,C), write('E'));
      (mapRadar(R,C,_), write('R'));
      (mapMedicine(R,C,_), write('M'));
      (mapFood(R,C,_), write('F'));
      (mapWeapon(R,C,_), write('W'));
      (write('P'))
      ), write(' '));
  write('')),
  Cpp is C + 1,
  printRow(T, R, Cpp).

look :- matrix(M), printMatrix(M), !.

printMap(M) :- printRs(M, 0).
printRs([], _).
printRs([H|T], R) :-
  printR(H, R, 0),
  Rpp is R + 1,
  printRs(T, Rpp).
printR([], _, _) :- nl.
printR([_|T], R, C) :-
  ((i_am_at(R,C), write('P'));
    (mapEnemy(R,C), write('E'));
    (mapMedicine(R,C,_), write('M'));
    (mapFood(R,C,_), write('F'));
    (mapWeapon(R,C,_), write('W'));
    (write('-'))
  ),
  write(' '),
  Cpp is C + 1,
  printR(T, R, Cpp).


map :-
    inventory(List),
    isInInventory('radar', List),
    matrix(M),
    printMap(M), !.

map :-
    write('You can''t use this, find the radar first.').

at(Value,R,C) :-
  matrix(M),
  AtR is R + 1,
  AtC is C + 1,
  nth(AtR, M, Row),
  nth(AtC, Row, Value).


randomenemy(0) :- !.
randomenemy(N) :-
            retract(mapEnemy(X,Y)),
            random(-1,2,Rx),
            TempX is Rx + X,
            (at('X',TempX,Y) -> (NewX is X); (NewX is TempX)),
            random(-1,2,Ry),
            TempY is Ry + Y,
            (at('X',NewX,TempY) -> (NewY is Y); (NewY is TempY)),
            assertz(mapEnemy(NewX,NewY)),
            NewN is N - 1,
            randomenemy(NewN).
w :-
  n(A,B),
  at('X',A,B),
  write('You can''t go there'),
  nl, 
  current_Hunger(Hunger),
  current_Thirst(Thirst),
  New_Hunger is Hunger - 1,
  New_Thirst is Thirst - 1,

  ((New_Thirst < 1, write('Do you think you are Superman? You cant live if you dont drink! Sorry, man.'), halt);
  asserta(current_Thirst(New_Thirst))),
  ((New_Hunger < 1, write('Do you think you are Superman? You run out of energy if you dont eat! Sorry, dude.'), halt);
  asserta(current_Hunger(New_Hunger))), !.

w :-
  enemyCount(N),
  randomenemy(N),

  retract(i_am_at(R, C)),

  retract(nw(_,_)),
  retract(n(_,_)),
  retract(nea(_,_)),
  retract(we(_,_)),
  retract(e(_,_)),
  retract(sw(_,_)),
  retract(se(_,_)),
  retract(so(_,_)),

  New_R is R-1,

  New_nw1 is New_R-1,
  New_nw2 is C-1,
  New_n1 is New_R-1,
  New_n2 is C,
  New_nea1 is New_R-1,
  New_nea2 is C+1,
  New_we1 is New_R,
  New_we2 is C-1,
  New_e1 is New_R,
  New_e2 is C+1,
  New_sw1 is New_R+1,
  New_sw2 is C-1,
  New_se1 is New_R+1,
  New_se2 is C+1,
  New_so1 is New_R+1,
  New_so2 is C,
  current_Hunger(Hunger),
  current_Thirst(Thirst),
  New_Hunger is Hunger - 1,
  New_Thirst is Thirst - 1,

  ((New_Thirst < 1, write('Do you think you are Superman? You cant live if you dont drink! Sorry, man.'), halt);
  asserta(current_Thirst(New_Thirst))),
  ((New_Hunger < 1, write('Do you think you are Superman? You run out of energy if you dont eat! Sorry, dude.'), halt);
  asserta(current_Hunger(New_Hunger))), !,
  asserta(nw(New_nw1,New_nw2)),
  asserta(n(New_n1,New_n2)),
  asserta(nea(New_nea1,New_nea2)),
  asserta(we(New_we1,New_we2)),
  asserta(e(New_e1,New_e2)),
  asserta(sw(New_sw1,New_sw2)),
  asserta(se(New_se1,New_se2)),
  asserta(so(New_so1,New_so2)),
  asserta(i_am_at(New_R, C)), 


  printtempat(New_R, C), !,
  printBarang(New_R, C), !.

a :-
  we(A,B),
  at('X', A, B),
  write('You can''t go there'),
  nl, 
  current_Hunger(Hunger),
  current_Thirst(Thirst),
  New_Hunger is Hunger - 1,
  New_Thirst is Thirst - 1,

  ((New_Thirst < 1, write('Do you think you are Superman? You cant live if you dont drink! Sorry, man.'), halt);
  asserta(current_Thirst(New_Thirst))),
  ((New_Hunger < 1, write('Do you think you are Superman? You run out of energy if you dont eat! Sorry, dude.'), halt);
  asserta(current_Hunger(New_Hunger))), !.

a :-
  enemyCount(N),
  randomenemy(N),

  retract(i_am_at(R, C)),

  retract(nw(_,_)),
  retract(n(_,_)),
  retract(nea(_,_)),
  retract(we(_,_)),
  retract(e(_,_)),
  retract(sw(_,_)),
  retract(se(_,_)),
  retract(so(_,_)),

  New_C is C-1,

  New_nw1 is R-1,
  New_nw2 is New_C-1,
  New_n1 is R-1,
  New_n2 is New_C,
  New_nea1 is R-1,
  New_nea2 is New_C+1,
  New_we1 is R,
  New_we2 is New_C-1,
  New_e1 is R,
  New_e2 is New_C+1,
  New_sw1 is R+1,
  New_sw2 is New_C-1,
  New_se1 is R+1,
  New_se2 is New_C+1,
  New_so1 is R+1,
  New_so2 is New_C,
  current_Hunger(Hunger),
  current_Thirst(Thirst),
  New_Hunger is Hunger - 1,
  New_Thirst is Thirst - 1,

  ((New_Thirst < 1, write('Do you think you are Superman? You cant live if you dont drink! Sorry, man.'), halt);
  asserta(current_Thirst(New_Thirst))),
  ((New_Hunger < 1, write('Do you think you are Superman? You run out of energy if you dont eat! Sorry, dude.'), halt);
  asserta(current_Hunger(New_Hunger))), !, 
  asserta(nw(New_nw1,New_nw2)),
  asserta(n(New_n1,New_n2)),
  asserta(nea(New_nea1,New_nea2)),
  asserta(we(New_we1,New_we2)),
  asserta(e(New_e1,New_e2)),
  asserta(sw(New_sw1,New_sw2)),
  asserta(se(New_se1,New_se2)),
  asserta(so(New_so1,New_so2)),
  asserta(i_am_at(R, New_C)), 

  printtempat(R, New_C), !,
  printBarang(R, New_C), !.

s :-
  so(A,B),
  at('X', A, B),
  write('You can''t go there'),
  nl, 
  current_Hunger(Hunger),
  current_Thirst(Thirst),
  New_Hunger is Hunger - 1,
  New_Thirst is Thirst - 1,

  ((New_Thirst < 1, write('Do you think you are Superman? You cant live if you dont drink! Sorry, man.'), halt);
  asserta(current_Thirst(New_Thirst))),
  ((New_Hunger < 1, write('Do you think you are Superman? You run out of energy if you dont eat! Sorry, dude.'), halt);
  asserta(current_Hunger(New_Hunger))), !.

s :-
  enemyCount(N),
  randomenemy(N),

  retract(i_am_at(R, C)),

  retract(nw(_,_)),
  retract(n(_,_)),
  retract(nea(_,_)),
  retract(we(_,_)),
  retract(e(_,_)),
  retract(sw(_,_)),
  retract(se(_,_)),
  retract(so(_,_)),

  New_R is R+1,

  New_nw1 is New_R-1,
  New_nw2 is C-1,
  New_n1 is New_R-1,
  New_n2 is C,
  New_nea1 is New_R-1,
  New_nea2 is C+1,
  New_we1 is New_R,
  New_we2 is C-1,
  New_e1 is New_R,
  New_e2 is C+1,
  New_sw1 is New_R+1,
  New_sw2 is C-1,
  New_se1 is New_R+1,
  New_se2 is C+1,
  New_so1 is New_R+1,
  New_so2 is C,
  current_Hunger(Hunger),
  current_Thirst(Thirst),
  New_Hunger is Hunger - 1,
  New_Thirst is Thirst - 1,

  ((New_Thirst < 1, write('Do you think you are Superman? You cant live if you dont drink! Sorry, man.'), halt);
  asserta(current_Thirst(New_Thirst))),
  ((New_Hunger < 1, write('Do you think you are Superman? You run out of energy if you dont eat! Sorry, dude.'), halt);
  asserta(current_Hunger(New_Hunger))), !,
  asserta(nw(New_nw1,New_nw2)),
  asserta(n(New_n1,New_n2)),
  asserta(nea(New_nea1,New_nea2)),
  asserta(we(New_we1,New_we2)),
  asserta(e(New_e1,New_e2)),
  asserta(sw(New_sw1,New_sw2)),
  asserta(se(New_se1,New_se2)),
  asserta(so(New_so1,New_so2)),
  asserta(i_am_at(New_R, C)), 

  printtempat(New_R, C), !,
  printBarang(New_R, C), !.

d :-
  e(A,B),
  at('X', A, B),
  write('It is a cliff. You can''t go there!'),
  nl, 
  current_Hunger(Hunger),
  current_Thirst(Thirst),
  New_Hunger is Hunger - 1,
  New_Thirst is Thirst - 1,

  ((New_Thirst < 1, write('Do you think you are Superman? You cant live if you dont drink! Sorry, man.'), halt);
  asserta(current_Thirst(New_Thirst))),
  ((New_Hunger < 1, write('Do you think you are Superman? You run out of energy if you dont eat! Sorry, dude.'), halt);
  asserta(current_Hunger(New_Hunger))), !.

d :-
  enemyCount(N),
  randomenemy(N),
  
  retract(i_am_at(R, C)),

  retract(nw(_,_)),
  retract(n(_,_)),
  retract(nea(_,_)),
  retract(we(_,_)),
  retract(e(_,_)),
  retract(sw(_,_)),
  retract(se(_,_)),
  retract(so(_,_)),

  New_C is C+1,

  New_nw1 is R-1,
  New_nw2 is New_C-1,
  New_n1 is R-1,
  New_n2 is New_C,
  New_nea1 is R-1,
  New_nea2 is New_C+1,
  New_we1 is R,
  New_we2 is New_C-1,
  New_e1 is R,
  New_e2 is New_C+1,
  New_sw1 is R+1,
  New_sw2 is New_C-1,
  New_se1 is R+1,
  New_se2 is New_C+1,
  New_so1 is R+1,
  New_so2 is New_C,
  current_Hunger(Hunger),
  current_Thirst(Thirst),
  New_Hunger is Hunger - 1,
  New_Thirst is Thirst - 1,

  ((New_Thirst < 1, write('Do you think you are Superman? You cant live if you dont drink! Sorry, man.'), halt);
  asserta(current_Thirst(New_Thirst))),
  ((New_Hunger < 1, write('Do you think you are Superman? You run out of energy if you dont eat! Sorry, dude.'), halt);
  asserta(current_Hunger(New_Hunger))), !,
  asserta(nw(New_nw1,New_nw2)),
  asserta(n(New_n1,New_n2)),
  asserta(nea(New_nea1,New_nea2)),
  asserta(we(New_we1,New_we2)),
  asserta(e(New_e1,New_e2)),
  asserta(sw(New_sw1,New_sw2)),
  asserta(se(New_se1,New_se2)),
  asserta(so(New_so1,New_so2)),
  asserta(i_am_at(R, New_C)), 

  printtempat(R, New_C), !,
  printBarang(R, New_C), !.

/* replace(M, R, C, NewElmt, M) */
replace([L|Ls], 0, Y, Z, [R|Ls]) :-
  replace_column(L,Y,Z,R).

replace([L|Ls], X, Y, Z, [L|Rs]) :-
  X1 is X-1,
  replace(Ls, X1, Y, Z, Rs).

replace_column([_|Cs], 0, Z, [Z|Cs]).
replace_column([C|Cs], Y, Z, [C|Rs] ) :-
  Y1 is Y-1,
  replace_column(Cs, Y1, Z, Rs).


take(I) :-
        i_am_at(X, Y),
        ((mapRadar(X,Y,I), radar(I), retract(mapRadar(X,Y,_)));
          (mapWeapon(X, Y, I), weapon(I), retract(mapWeapon(X,Y,_)));
         (mapFood(X, Y, I), food(I, _), retract(mapFood(X,Y,_)));
         (mapMedicine(X, Y, I), medicine(I, _), retract(mapMedicine(X,Y,_)))),
        retract(inventory(List)),
        asserta(inventory([I|List])),
        write(I), write(' have been taken'),
        nl, !.

take(_) :-
        write('I don''t see anything here. Focus!'),
        nl.


dropItem(_,[],[]).
dropItem(X,[X|Xs],Xs).
dropItem(X,[Y|Xs],[Y|Ys]) :-
                          X \= Y,
                          dropItem(X,Xs,Ys).


use(I) :- 
        radar(I), write('You can see the whole world.'), !.

use(I) :-
       inventory(List),
       isInInventory(I, List),
       retract(inventory(_)),
       dropItem(I, List, New_List),
       asserta(inventory(New_List)),
       ((weapon(I), retract(hands(_)), asserta(hands(I)), write(I), write(' ready to use.'));
        (medicine(I, X), retract(current_Health(Current_Health)), New_Health is Current_Health + X, ((New_Health > 100, asserta(current_Health(100)));
          (asserta(current_Health(New_Health)))), write(I), write(' has been used.'));
        (food(I, X),retract(current_Hunger(Current_Hunger)), New_Hunger is Current_Hunger + X, ((New_Hunger > 100, asserta(current_Hunger(100)));
          (asserta(current_Hunger(New_Hunger)))), write(I), write(' has been eaten.'));
        (I = water_pouch,retract(current_Thirst(Current_Thirst)), New_Thirst is Current_Thirst + 40, ((New_Thirst > 100, asserta(current_Thirst(100)));
          (asserta(current_Thirst(New_Thirst)))), write(I), write(' has been used. The pouch is empty. Find the lake if you want to drink again!'),asserta(inventory([pouch|New_List])));
        (I = pouch, i_am_at(X,Y), ((water(X,Y), write(I), write(' has been used. The pouch has been filled.'), asserta(inventory([water_pouch|New_List]))); write('you cant use it here, Go find a lake.'),asserta(inventory([pouch|New_List]))))
       ),
       nl, !.

use(_) :-
       write('Are you sure? You don''t have it!'),
       nl.

printBarang(X,Y) :-
              ((mapRadar(X,Y,I), write('You see an '), write(I), write(' on the ground.'));
                (mapFood(X,Y,I), write('You see an '), write(I), write(' on the ground.'));
                (mapWeapon(X,Y,I), write('You see an '), write(I), write(' on the ground.'));
                (mapMedicine(X,Y,I), write('You see an '), write(I), write(' on the ground.'));
                (mapEnemy(X,Y), write('You see an enemy nearby.'));
                (write(''))
                ).    

printnamatempat(X,Y):- 
          ((at('G',X,Y),write('an open field')); 
           (at('C',X,Y),write('the cave')); 
           (at('J',X,Y),write('the jungle')); 
           (at('M',X,Y),write('the mountain')); 
           (at('D',X,Y),write('the desert')); 
           (at('X',X,Y),write('the cliff'));
           (at('L',X,Y),write('the lake')) 
          ), !. 

printtempat(X,Y) :- 
          write('You are at '), 
          ((at('G',X,Y),write('an open field. ')); 
           (at('C',X,Y),write('the cave. ')); 
           (at('J',X,Y),write('the jungle. ')); 
           (at('M',X,Y),write('the mountain. ')); 
           (at('D',X,Y),write('the desert. ')); 
           (at('X',X,Y),write('the cliff. '));
           (at('L',X,Y),write('the lake. ')) 
          ), nl,
          write('To the north is '), 
          printnamatempat(X+1, Y),write('; '), nl,
          write('To the west is '), 
          printnamatempat(X, Y-1),write('; '), nl,
          write('To the east is '), 
          printnamatempat(X, Y+1),write('; '), nl,
          write('To the south is '), 
          printnamatempat(X-1, Y),write('.'),nl.

attack :-
  i_am_at(X, Y),
  mapEnemy(X,Y),
  retract(current_Health(Current_Health)),
  retract(damage(Damage)),
  New_Health is Current_Health - Damage,
  New_Damage is Damage + 2,
  ((New_Health < 1, write('You died with honour. Goodbye, sir!'),nl,halt);
    (asserta(current_Health(New_Health)))
    ),
  asserta(damage(New_Damage)),
  hands(none),
  write('You don''t have any weapon on hand'), nl, !.

attack :-
  i_am_at(X, Y),
  mapEnemy(X,Y),
  retract(current_Health(Current_Health)),
  retract(damage(Damage)),
  New_Health is Current_Health - Damage,
  New_Damage is Damage + 2,
  ((New_Health < 1, write('You died with honour. Goodbye, sir!'),nl,halt);
    (asserta(current_Health(New_Health)))
    ),
  asserta(damage(New_Damage)),
  hands(Weapon),
  weapon(Weapon),
  write('You took '),
  write(Damage),
  write(' damage! You have killed an enemy.'),
  retract(mapEnemy(X,Y)),
  asserta(damage(New_Damage)), 
  retract(enemyCount(C)),
  New_C is C - 1,
  ((New_C < 1, write('You heard the horn sound, you are very familiar with it. Every year you would hear the exact sound when watching the Hunger Games in your district. The horn signals the end of the Hunger Games.'), nl,
    write('You have won the Hunger Games!'), nl, halt);
  (asserta(enemyCount(New_C)))), !.

attack :-
  write('There is nobody here. Nobody in your vision, anyway.').


isInInventory(X,[X|_]).
isInInventory(X,[Y|Xs]) :-
                      X \= Y,
                      isInInventory(X,Xs).

printInventory([]).
printInventory([H|T]) :-
                      write(H),
                      printInventory(T).

drop(I) :-
        hands(I),
        retract(hands(I)),
        asserta(hands(none)),
        write(I), write(' have been dropped.'),
        i_am_at(X,Y),
        ((weapon(I), asserta(mapWeapon(X,Y,I)));
          (food(I,_), asserta(mapFood(X,Y,I)));
          (medicine(I,_), asserta(mapMedicine(X,Y,I)))
          ),
        nl, !.

drop(I) :-
        inventory(List),
        retract(inventory(_)),
        isInInventory(I, List),
        dropItem(I, List, New_List),
        asserta(inventory(New_List)),
        write(I), write(' have been dropped.'),
        i_am_at(X,Y),
        ((weapon(I), asserta(mapWeapon(X,Y,I)));
          (food(I,_), asserta(mapFood(X,Y,I)));
          (medicine(I,_), asserta(mapMedicine(X,Y,I)))
          ),
        nl, !.

drop(_) :-
        write('You don''t have it!'),
        nl.

save(Filename) :-
          open(Filename, write, S),
          set_output(S), 
          write(S,':- dynamic(current_Health/1).\n'),
          write(S,':- dynamic(current_Hunger/1).\n'),
          write(S,':- dynamic(current_Thirst/1).\n'),
          write(S,':- dynamic(at/3).\n'),
          write(S,':- dynamic(damage/1).\n'),
          write(S,':- dynamic(i_am_at/2).\n'),
          write(S,':- dynamic(nw/2).\n'),
          write(S,':- dynamic(n/2).\n'),
          write(S,':- dynamic(nea/2).\n'),
          write(S,':- dynamic(we/2).\n'),
          write(S,':- dynamic(e/2).\n'),
          write(S,':- dynamic(sw/2).\n'),
          write(S,':- dynamic(se/2).\n'),
          write(S,':- dynamic(so/2).\n'),
          write(S,':- dynamic(matrix/1).\n'),
          write(S,':- dynamic(inventory/1).\n'),
          write(S,':- dynamic(hands/1).\n'),
          write(S,':- dynamic(mapWeapon/3).\n'),
          write(S,':- dynamic(mapFood/3).\n'),
          write(S,':- dynamic(mapMedicine/3).\n'),
          write(S,':- dynamic(mapEnemy/2).\n'),
          write(S,':- dynamic(mapRadar/3).\n'),
          write(S,':- dynamic(enemyCount/1).\n'),
          listing, close(S).

loadd(Filename) :-
          consult(Filename).