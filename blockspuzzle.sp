%%%%% BlocksPuzzle domain; additional properties in a standard
%%%%% blocks-world domain...

#const n = 3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sorts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#block = [b][0..2].
#location = #block + {table}.

%% Static attributes of blocks...
#color = {red, green, blue}.
#shape = {cube, cuboid, prism}.
#size = {small, medium, large}.

%% Fluents...
#inertial_fluent = on(#block(X),#location(Y)):X!=Y.
#fluent = #inertial_fluent.

%% Actions...
#action = move(#block(X),#location(Y)):X!=Y.

#step = 0..n.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Block attributes...
has_color(#block, #color).
has_shape(#block, #shape).
has_size(#block, #size).

holds(#fluent,#step).
occurs(#action,#step).

obs(#fluent, #step).
hpd(#action, #step).

success().
goal(#step).
something_happened(#step).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Causal laws %%

%% Putting block B on location L at step I
%% causes B to be on L at step I+1:
%% move(B,L) causes on(B,L)
holds(on(B,L),I+1) :- occurs(move(B,L),I).


%% State constraints %%

%% A block can only be in one location at a time...
%% -on(B,L2) if on(B,L1), L1 != L2
-holds(on(B,L2),I) :- holds(on(B,L1),I), 
                      L1 != L2.

%% Only one block can be on top of another block...
%% -on(B2,B) if on(B1,B), B1 != B2
-holds(on(B2,B),I) :- #block(B), 
                      holds(on(B1,B),I),
                      B1 != B2.

%% Object attributes can only have one value each...
-has_color(B, C2) :- has_color(B, C1), C1 != C2.
-has_shape(B, S2) :- has_shape(B, S1), S1 != S2.
-has_size(B, Z2) :- has_size(B, Z1), Z1 != Z2.


%% If block B is not known to be at location L at step 0,
%% assume it is not there; reasoning with "unknown"s ...
%-holds(on(B,L),0) :- not holds(on(B,L),0). 


%% Executability conditions %%

%% Cannot move a block that is under another block...       
%% impossible move(B,L) if on (B1,B)
-occurs(move(B,L),I) :- holds(on(B1,B),I).

%% Cannot move a block onto a block that is occupied...
%% impossible move(B1,B) if on(B2,B).  
-occurs(move(B1,B),I) :- #block(B),
                        holds(on(B2,B),I).

%% Cannot move a block to a location it is already on...
%% impossible move(B, L) if on (B, L).
-occurs(move(B, L), I) :- holds(on(B, L), I).



%% Axioms to be learned --inserted here for testing...
-occurs(move(B2,B1),I) :- #block(B1), has_shape(B1, prism).
-occurs(move(B2,B1),I) :- #block(B1), has_size(B1, small), 
				has_size(B2, large).


%% General inertia axioms
holds(F,I+1) :- #inertial_fluent(F),
                holds(F,I),
                not -holds(F,I+1).

-holds(F,I+1) :- #inertial_fluent(F),
                 -holds(F,I),
                 not holds(F,I+1).
                 
%% CWA for Actions
-occurs(A,I) :- not occurs(A,I).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%       
%% Default and CR rules

%% Blocks are on the table until stated otherwise...
holds(on(B, table), 0) :- #block(B),
 			  not -holds(on(B, table), 0).

-holds(on(B, table), 0) :+ #block(B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%       
%% History rules

%% Take what actually happened into account...
occurs(A,I) :- hpd(A,I).

%% Reality check axioms...
:- obs(F, I), -holds(F, I).
:- -obs(F, I), holds(F, I).

                
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%       
%% Planning module...

%% Failure is not an option...
success :- goal(I).
:- not success.

%% Have to keep executing actions until goal is achieved...
occurs(A,I) | -occurs(A,I) :- not goal(I).
                              
%% Cannot execute two actions at the same time...
:- occurs(A1,I), occurs(A2,I), A1 != A2.

%% An action occurs at each step before goal is achieved...
something_happened(I) :- occurs(A,I).

:- not goal(I),
   not something_happened(I).

%:- goal(I), goal(I-1),
%   J < I,
%   not something_happened(J).
                                        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Initial state for a domain with three blocks...
%holds(on(b0,table),0).                         
%holds(on(b1,table),0).
%holds(on(b2,table),0).

%% Include block properties...
has_color(b0, red).
has_shape(b0, cube).
has_size(b0, medium).

has_color(b1, green).
has_shape(b1, cuboid).
has_size(b1, medium).

has_color(b2, blue).
has_shape(b2, prism).
has_size(b2, large).
       
obs(on(b1, b0), 2).

       
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Goal to be achieved; stack blocks... 
goal(I) :- holds(on(B1,table),I),  holds(on(B2,B1),I),  holds(on(B3, B2),I).
                     
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
display
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
occurs.
%holds.
