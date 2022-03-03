
(** HW2 **)

(** Problem 1 **)

fun same_string (s1 : string, s2 : string) =
    s1 = s2;

(* 1a
Removes the string from the list if it is there, NONE if not there
 *)

fun all_except_option (str, lst) =
    case lst of
	[] => NONE
      | x::xs' => if same_string (str, x)
		  then SOME xs'
		  else case all_except_option (str, xs') of
			   NONE => NONE
			 | SOME xss' => SOME (x::xss');

(* 1b
Produces a list from substitions that include s
ASSUME each list in substitutions has no repeats
*)

fun get_substitutions1 (substitutions, str) =
    case substitutions of
	[] => []
      | x::xs' => case all_except_option (str, x) of
			  NONE => get_substitutions1 (xs', str)
			| SOME xs => xs @ get_substitutions1 (xs', str)

(* 1c
Tail-Recursive version of get_substitutions1
 *)

fun get_substitutions2 (substitutions0, str) =
    let fun helper (substitutions, acc) =
	    case substitutions of
		 []  => acc
	       | x::xs' => case all_except_option (str, x) of
			       NONE => helper (xs', acc)
			     | SOME xs => helper (xs', xs @ acc) 
    in
	helper (substitutions0, [])
    end;


(* 1d
Produces all the full names you can produce by substituting the first name
 *)

fun similar_names (strlist, name) =
    let fun get_subs (name) =
	    case name of 
		{first=x, middle=y, last=z} => get_substitutions1(strlist, x)
	fun helper subslist =
	    case subslist of
		[] => []
	      | head :: rest => case name of
				    {first=x, last=y, middle=z} =>
				    [{first=head, last=y, middle=z}]
				    @ helper (rest)
    in
	name::helper (get_subs (name))
    end;


(** Problem 2 **)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove;

(* 2a
Produces the color of the card
 *)

fun card_color card =
    case card of
	(x, _) => if x = Hearts orelse x = Diamonds
		  then Red else Black;

(* 2b
Produces the value of the card
 *)

fun card_value card =
    case card of
	(_, x) => case x of
		      Ace => 11
		    | Num x => x
		    | _ => 10;

(* 2c
Removes the first occurence of card if in the card list, or exception
*)

fun remove_card (cardlist, card, e) =
    case cardlist of
	[] => raise e
      | x::xs' => if x = card then xs'
		  else x :: remove_card (xs', card, e);

(* 2d
Produces true if all the cards in the list are the same color
 *)

fun all_same_color cardlist =
    case cardlist of
	[] => true
      | _ :: [] => true
      | head::(neck::rest) => (card_color(head) = card_color(neck) andalso
			       all_same_color (neck::rest));  

(* 2e
Produces the sum of the values of all cards in the list
 *)

fun sum_cards cardlist =
    let fun helper (cardlist,acc) =
	    case cardlist of
		[] => acc
	      | x::xs' =>  helper (xs', (card_value(x) + acc))
    in
	helper(cardlist, 0)
    end;

(* 2f
Produces the score of the hand according to the rules of the game:
 *)

fun score (cardlist, goal) =
    let val sum = sum_cards (cardlist)
	val prelim_score = if sum > goal
			   then ((sum - goal) * 3)
			   else (goal-sum)
    in
	if all_same_color (cardlist)
	then prelim_score div 2
	else prelim_score
    end;

(* 2e
card list * move list * int (goal) => score
 *)

fun officiate (cards, moves, goal) =
    let fun helper (cards, moves, held) =
	    case moves of
		[] => held
	      | x::xs' => case x of
			      Discard x => helper (cards, xs', (remove_card (held, x, IllegalMove)))
			    | Draw => case cards of
					  [] => held
					| c::cs' => let val curr_held = (c::held)
						    in 
							case sum_cards(curr_held) > goal of
							    true => curr_held
							  | false => helper (cs', xs',curr_held)
						    end
    in
	score (helper (cards, moves, []), goal)
    end;


fun officiate_v2 (cards, moves, goal) =
    let
	fun play ([], _, held) = score(held, goal)
	  | play (_, [], held) = score(held, goal)
	  | play (card::cards, Draw::moves, held)  =
	    let
		val new_held = card::held
		val sum = sum_cards(new_held)
	    in
		if sum > goal then score (new_held, goal) else play (cards, moves, new_held)
	    end
	  | play (cards, (Discard card)::moves, held) =
	    play (cards, moves, remove_card (held, card, IllegalMove))
    in
	play (cards, moves, [])
    end;
