val x = {bar=3, foo=true, baz="abc"};

#foo x;

datatype mytype = TwoInts of int * int
		| Str of string
		| Pizza
		     

fun f y =
    case y of
	Pizza => 3
     |  TwoInts (i1, i2) => i1 + i2
     |  Str s  => String.size s

			      
datatype exp =
	 Constant of int
	 | Negate of exp
	 | Add of exp * exp
	 | Multiply of exp * exp;

fun eval e =
    case e of
	Constant i => i
     |  Negate e1  => ~(eval e1)
     |  Add (e1, e2)  => (eval e1) + (eval e2)
     |  Multiply (e1, e2) => (eval e1) * (eval e2);

val ans1 = eval (Negate (Constant 1));
val ans2 = eval (Add (Constant 10, Constant 20));
val ans3 = eval (Multiply (Constant 20, (Add (Constant 20, Constant 20))));

fun sum_list xs =
    case xs of
	[] => 0
     |  x::xs' => x + sum_list xs';

fun append (xs, ys) =
    case xs of
	[] => ys
     |  x::xs'  => x :: append (xs', ys);

fun sum_triple_1 triple =
    case triple of
	(x, y, z) => x + y + z;

fun sum_triple_2 triple =
    let val (x,y,z) = triple
    in
	x + y + z
    end;

fun sum_triple_3 (x, y, z) =
    x + y + z;

fun full_name_1 r =
    case r of
	{first=x, middle=y, last=z} => x ^ " " ^ y ^ " " ^ z;

fun full_name_2 r  =
    let val {first=x, middle=y, last=z} = r
    in
	x ^ " " ^ y ^ " " ^ z
    end;

fun full_name_3 {first=x, middle=y, last=z} =
    x ^ " " ^ y ^ " " ^ z;
