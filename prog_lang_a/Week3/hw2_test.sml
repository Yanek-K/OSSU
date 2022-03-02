(* Homework2 Simple Test *)
use "hw2.sml";

val test10 = all_except_option ("str", ["str"]) =  SOME [];
val test11 = all_except_option ("str", ["notstring"]) = NONE;
val test12 = all_except_option ("str", ["str", "notstring"]) = SOME ["notstring"];
val test13 = all_except_option ("str", ["not", "the", "one"]) = NONE
val test14 = all_except_option ("str", ["not", "one", "str"]) = SOME ["not", "one"];

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test20 = get_substitutions1 ([["Jeff"], ["Michael"]], "foo") = [];
val test21 = get_substitutions1 ([["foo", "fee"],["there"]], "foo") = ["fee"];
val test22 = get_substitutions1 ([["foo", "fee"],["there", "foo"]], "foo")
	     = ["fee", "there"];
val test23 = get_substitutions1 ([["Fred", "Frederick"],
				  ["Jeff", "Jeffrey"],
				  ["Jeff", "Geoff", "Jeffrey"]], "Jeff")
	     = ["Jeffrey", "Geoff", "Jeffrey"];


val test40 = similar_names ([["Fred","Fredrick"],
			    ["Elizabeth","Betty"],
			    ["Freddie","Fred","F"]], 
			    {first="Fred", middle="W", last="Smith"}) 
	= [{first="Fred", last="Smith", middle="W"}, 
	   {first="Fredrick", last="Smith", middle="W"},
	   {first="Freddie", last="Smith", middle="W"}, 
	   {first="F", last="Smith", middle="W"}];
val test41 = similar_names ([["Mike, Michael"]],
			    {first="Peter", last="Smith", middle="P"})
	     = [{first="Peter", last="Smith", middle="P"}];
val test42 = similar_names ([["Mike", "Michael"]],
			    {first="Mike", last="Smith", middle="P"})
	     = [{first="Mike", last="Smith", middle="P"},
		{first="Michael", last="Smith", middle="P"}];
val test43 = similar_names ([["Mike, Michael"], ["Pete", "Peter"]],
			    {first="Mike", last="Smith", middle="P"})
	     = [{first="Mike", last="Smith", middle="P"}];
val test44 = similar_names ([["Mike", "Michael"], ["Mike", "Les", "King"]],
			    {first="Mike", last="Smith", middle="P"})
	     = [{first="Mike", last="Smith", middle="P"},
		{first="Michael", last="Smith", middle="P"},
		{first="Les", last="Smith", middle="P"},
		{first="King", last="Smith", middle="P"}];
(*
val test5 = card_color (Clubs, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
         *)    
             
