
sits_left_of(tywin_lannister, cersei_baratheon).
sits_left_of(cersei_baratheon, janos_slynt).
sits_left_of(janos_slynt, tyrion_lannister).
sits_left_of(tyrion_lannister, grand_master_pycelle).
sits_left_of(grand_master_pycelle, varys).
sits_left_of(varys, petyr_baelish).
sits_left_of(petyr_baelish, tywin_lannister).

sits_right_of(X, Y) :- sits_left_of(Y, X).

are_neighbors_of(X, Y, Z) :- sits_left_of(X, Z), sits_right_of(Y, Z).

next_to_each_other(X, Y) :- sits_left_of(X, Y); sits_right_of(X, Y).

% ?- sits_right_of(petyr_baelish, cersei_baratheon). -> false
% ?- sits_right_of(petyr_baelish, varys). -> true
% ?- sits_right_of(janos_slynt, Y). -> Y = cersei_baratheon
% ?- are_neighbors_of(cersei_baratheon, X, Y). -> X = tyrion_lannister
% ?- are_neighbors_of(grand_master_pycelle, petyr_baelish, X). -> X = varys
