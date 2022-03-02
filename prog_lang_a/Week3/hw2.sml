
(** HW2 **)

(** Problem 1 **)

fun same_string(s1 : string, s2 : string) =
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
      | head::rest => if isSome (all_except_option (str, head))
		      then valOf (all_except_option (str, head))
			   @ get_substitutions1 (rest, str)
		      else get_substitutions1 (rest, str);

(* 1c
Tail-Recursive version of get_substitutions1
 *)

fun get_substitutions2 (substitutions0, str) =
    let fun helper (substitutions, acc) =
	    case substitutions of
		 []  => acc
	       | head :: rest => if isSome (all_except_option (str, head))
				 then
				     helper (rest,
					     valOf (all_except_option (str, head))@acc)
				 else helper (rest, acc)
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

(*
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
*)
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

fun remove_card (card_list, card, e) =
    case card_list of
	[] => raise e
      | head::rest => if head = card then rest
		      else head :: remove_card (card_list, card, e);

(* 2d
card list => Boolean
Produces true if all the cards in the list are the same color
 *)

fun all_same_color card_list =
    case card_list of
	[] => true
      | _ :: [] => true
      | head::(neck::rest) => (card_color(head) = card_color(neck) andalso
			       all_same_color (neck::rest));  


val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true	
