(** Part One **)

(* Datatypes *)
type student_id = int;
type grade = int (* must be in 0 - 100 range *);
type final_grade = { id : student_id, grade : grade option};
datatype pass_fail = pass | fail;

(* #1 Produces pass if the grade is >= 75, else fail *)
fun pass_or_fail student =
    case student of
	{ id = _, grade = SOME grade} => if grade >= 75 then pass else fail
     |  _ => fail; 

(* #2 Produces true if the grade field contains SOME i for an i >= 75 *)
fun has_passed student =
    case pass_or_fail student of
	pass => true
     |  _  => false; 
	
(* #3 Produces the number of final_grades have a passing mark (>= 75) *)
fun number_passed student =
    case student of
	x::students => (if has_passed (x) then 1 else 0) + number_passed (students)
      | []  => 0;


(* #4b Produces the number of elements where a pair has passed but x is a fail,
       or vice versa *)
fun number_misgraded (pair_list) =
    case pair_list of
	[] => 0
      | (pass, x)::rest => (if has_passed (x) then 0 else 1)
			   + number_misgraded (rest)
      | (fail, x)::rest => (if has_passed (x) then 1 else 0)
			   + number_misgraded(rest);

(** Part Two **)

(* Datatypes *)

datatype 'a tree =
	 leaf
	 | node of { value: 'a,
		     left : 'a tree,
		     right : 'a tree };

datatype flag = leave_me_alone | prune_me;

(* #5 Produces the length of the longest path to a leaf (height) *)
fun tree_height tree =
    case tree of
	 leaf => 0
       | node {value, left, right} => 1 +
				     Int.max (tree_height (left),
					      tree_height (right));
       
(* #6 Produces the sum of all the values in the nodes *)
fun sum_tree tree =
    case tree of
	leaf => 0 
      | node {value, left, right} => value
				     + sum_tree (left)
				     + sum_tree (right);

(* #7 Produces a tree where all nodes containing prune_me replaced with a leaf *)
fun gardener tree =
    case tree of
	node {value=leave_me_alone,
	      left = ltree,
	      right = rtree} => node { value = leave_me_alone,
				       left = gardener (ltree),
				       right = gardener (rtree)}
      | _ => leaf;

(* #8a Produces the last element in the list, null if empty *)
exception Empty;

fun mylast input_list =
    case input_list of
	[] => raise Empty
      | x::[] => x
      | x::xs' => mylast (xs');

(* #8b Produces the first i elements of the list
       Raises Subscript error if i < 0 or i > length list *)
fun mytake (xs,i) =
    if i < 0 then raise Subscript
    else
	case xs of
	    [] => []
	 |  (x::xs') => if i > 0
			then x :: mytake (xs', i-1)
			else []; 

(* #8c Produces what is left after dropping the first i elements of the list l *)
fun mydrop (xs, i) =
    if i < 0 then raise Subscript
    else if i = 0 then xs
    else
	case xs of
	    [] => []
	  | (_::xs') => mydrop (xs', i - 1);
	
(* #8d Produces the list that is the concatenation of all the list in l *)
fun myconcat xss =
    case xss of
	[] => []
      | (xs::xss') => xs @ myconcat (xss');

(** Part Three **)

(* Datatypes *)

datatype nat = ZERO | SUCC of nat;

(* A "nat" number is either zero or the "successor" of another integer.
   E.g. - 2 is SUCC (SUCC ZERO) *)

(* 9 Produces true if the number is positive (i.e. not zero) *)
fun is_positive nat =
    case nat of
	ZERO => false
      | _ => true; 

(* 10 Produces the predecessor to the "natural" *)
exception Negative
	      
fun pred nat =
    case nat of
	ZERO => raise Negative
      | SUCC(nat') => (nat');

(* 11 Produces the corresponding int
      E.g. - SUCC (SUCC ZERO)) = 2 *)
fun nat_to_int nat =
    case nat of
	ZERO => 0
      | SUCC (nat') => 1 + nat_to_int (nat'); 				    

(* 12 Produces a "natural" representation of the int,
      Exception if the integer was negatve *)
fun int_to_nat n =
    if n < 0 then raise Negative
    else
    case n of
	0 => ZERO
      | n => SUCC(int_to_nat (n - 1)); 

(* 13 Produces the sum of the two args *)
fun add (nat1, nat2) =
    case nat2 of
	ZERO => nat1
      | SUCC (nat2') => add ( SUCC(nat1), (nat2')); 

(* 14 Subtracts nat1 from nat2 *)
fun sub (nat1, nat2) =
    case nat2 of
	ZERO => nat1
      | SUCC (nat2') => sub (pred(nat1), nat2');

(* 16 Produces True if nat1 < nat2 *)
fun less_than nats =
    case nats of
	(_, ZERO) => false
      | (ZERO, _) => true
      | (SUCC(nat1'),SUCC(nat2')) => less_than (nat1', nat2'); 
