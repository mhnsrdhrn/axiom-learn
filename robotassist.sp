#const numSteps = 4.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  sorts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#location = {office, library, kitchen, workshop}.
#role = {engineer, sales, manager}.
#status = {damaged, good}.
#weight = {light, heavy}.

#robot = {rob0}.
#person = {per0}.
#entity = #robot + #person.	

#textbook = {text0}.
#object = #textbook.

#thing = #object + #entity.

#boolean = {true, false}.
#step = 0..numSteps.

%% Fluents
#inertial_fluent = loc(#thing, #location) + in_hand(#entity, #object).
#fluent = #inertial_fluent.
#action = move(#robot, #location) + pickup(#robot, #object) 
	+ putdown(#robot, #object) + serve(#robot, #object, #person).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
role_type(#person, #role).
obj_status(#object, #status).
obj_weight(#object, #weight).

is_defined(#fluent).

holds(#fluent,#step).
occurs(#action,#step).

obs(#fluent, #boolean, #step).
hpd(#action, #step).

success().
goal(#step). 
something_happened(#step).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 rules			        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Causal Laws %%
%% Moving changes location to target room...
holds(loc(R, L), I+1) :- occurs(move(R, L), I).

%% Grasping an object causes object to be in hand...
holds(in_hand(R, O), I+1) :- occurs(pickup(R, O), I). 

%% Putting an object down causes it to no longer be in hand...
-holds(in_hand(R, O), I+1) :- occurs(putdown(R, O), I). 

%% Serving an object to a human causes the object to be in human's hand...
holds(in_hand(P, O), I+1) :- occurs(serve(R, O, P), I).

%% Serving an object causes the object to not be in robot's hand...
-holds(in_hand(R, O), I+1) :- occurs(serve(R, O, P), I).


%% State Constraints %%
%% Any object exists in only one location...
-holds(loc(Th, L2), I) :- #thing(Th), holds(loc(Th, L1), I), L1!=L2.

%% If a robot is holding an object, they have the same location...
holds(loc(O, L), I) :- holds(loc(R, L), I), holds(in_hand(R, O), I).

%% Only one entity can have an object in hand...
-holds(in_hand(E2, O), I) :- holds(in_hand(E1, O), I), E1 != E2.

%% Only one object can be held at any time...
-holds(in_hand(E, O2), I) :- holds(in_hand(E, O1), I), O1 != O2.

%% If thing is not at a location initially, assume not there ...
%-holds(loc(Th, L), 0) :- not holds(loc(Th, L),0), #thing(Th). 

%% Room type, status and weight have unique values...
-role_type(P, RT2) :- role_type(P, RT1), RT1 != RT2.
-obj_status(O, S2) :- obj_status(O, S1), S1 != S2.
-obj_weight(O, W2) :- obj_weight(O, W1), W1 != W2.


%% Executability Conditions %%
%% Cannot move to a location if you are already there...
-occurs(move(R, L), I) :- holds(loc(R, L), I).

%% Cannot pick up an object if you are not in the same room...
-occurs(pickup(R, O), I) :- holds(loc(R, L1), I), 
			    holds(loc(O, L2), I), L1 != L2.

%% Cannot pick up an object already in hand... 
-occurs(pickup(R, O), I) :- holds(in_hand(R, O), I).

%% Rules to prevent incorrect grasping...
-occurs(pickup(R, O), I) :- holds(loc(R, L), I), -holds(loc(O, L), I).
-occurs(pickup(R, O), I) :- holds(loc(O, L), I), -holds(loc(R, L), I).

%% Cannot put down an object unless it is in hand...
-occurs(putdown(R, O), I) :-  not holds(in_hand(R, O), I).

%% Cannot serve an object that is not in hand...
-occurs(serve(R, O, P), I) :- not holds(in_hand(R, O), I).

%% Cannot serve an object unless robot and human are in same location...
-occurs(serve(R, O, P), I) :- holds(loc(R, L1), I), 
			      holds(loc(P, L2), I), L1 != L2.

%% Rules to prevent incorrect serving...
-occurs(serve(R, O, P), I) :- holds(loc(R, L), I), -holds(loc(P, L), I).
-occurs(serve(R, O, P), I) :- holds(loc(P, L), I), -holds(loc(R, L), I).

%% Rules to be discovered...
-occurs(serve(R, O, P), I) :- role_type(P, engineer), 
			      holds(loc(P, workshop), I).
-occurs(serve(R, O, P), I) :- obj_status(O, damaged).
-occurs(pickup(R, O), I) :- obj_weight(O, heavy).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%       
%% Inertial axiom + CWA

%% General inertia axioms...
holds(F,I+1) :- #inertial_fluent(F),
                holds(F,I),
                not -holds(F,I+1).

-holds(F,I+1) :- #inertial_fluent(F),
                 -holds(F,I),
                 not holds(F,I+1).
                 
%% CWA for Actions...
-occurs(A,I) :- not occurs(A,I).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%       
%% Default and CR rules

%% Books are usually in the library...
holds(loc(B, library), 0) :- #textbook(B), 
			     not -holds(loc(B, library), 0).

%% Books are otherwise in the office...
holds(loc(B, office), 0) :- #textbook(B),
		       	     -holds(loc(B, library), 0),
 		             not -holds(loc(B, office), 0).

%-holds(loc(B, office), 0) :+ #textbook(B)
%		              -holds(loc(B, library), 0).


is_defined(loc(B, library)) :- #textbook(B).

%% Under exceptional circumstances, assume books are elsewhere...
-holds(loc(B, library), 0) :+ #textbook(B).
-holds(loc(B, office), 0) :+ #textbook(B), 
			     -holds(loc(B, library), 0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%       
%% History and initial state rules

%% Take what actually happened into account...
occurs(A,I) :- hpd(A,I).

%% Reality check axioms...
:- obs(F, true, I), -holds(F, I).
:- obs(F, false, I), holds(F, I).

is_defined(F) :- obs(F, Y, 0).
-holds(F, 0) :- #inertial_fluent(F),
		not is_defined(F), not holds(F, 0).

%% Awareness axiom...
%holds(F, 0) | -holds(F, 0) :- #inertial_fluent(F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%       
%% Planning Module

%% Failure is not an option...
success :- goal(I).
:- not success. 

%% Cannot be idle while goal remains unachieved...
occurs(A, I) | -occurs(A, I) :- not goal(I). 

%% Cannot execute two actions at the same time...
:- occurs(A1,I), occurs(A2,I), A1 != A2.

something_happened(I) :- occurs(A, I).
:- not goal(I),
   not something_happened(I).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%       
%% Initial state...
holds(loc(rob0, office), 0).
%holds(loc(text0, library), 0). 
holds(loc(per0, workshop), 0).

%obj_weight(text0, light).
%obj_status(text0, good).
%role_type(per0, engineer).

%obs(loc(text0, library), false, 1).
obs(loc(text0, workshop), true, 3).
%obs(in_hand(rob0, text0), false, 3).

%% Goal...
%goal(I) :- holds(loc(text0, office), I), -holds(in_hand(rob0, text0), I).
goal(I) :- holds(in_hand(per0, text0), I).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%       
display
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%       
occurs.
%holds.
