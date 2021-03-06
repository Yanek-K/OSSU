use "extra_problems_week3.sml";

(** Pass/Fail **)

(* Pass/Fail -- #1 *)
val test_pass_or_fail_1 = pass_or_fail { id = 1023, grade = SOME 73 } = fail
val test_pass_or_fail_2 = pass_or_fail { id = 1, grade = SOME 48 } = fail
val test_pass_or_fail_3 = pass_or_fail { id = 10231023, grade = SOME 0 } = fail
val test_pass_or_fail_4 = pass_or_fail { id = 1729, grade = NONE } = fail
val test_pass_or_fail_5 = pass_or_fail { id = 432, grade = SOME 74 } = fail
val test_pass_or_fail_6 = pass_or_fail { id = 2, grade = SOME 75 } = pass
val test_pass_or_fail_7 = pass_or_fail { id = 13, grade = SOME 100 } = pass
val test_pass_or_fail_8 = pass_or_fail { id = 15, grade = SOME 86 } = pass

								
(* Pass/Fail -- #2 *)
val test_has_passed_1 = has_passed { id = 1023, grade = SOME 73 } = false
val test_has_passed_2 = has_passed { id = 1, grade = SOME 48 } = false
val test_has_passed_3 = has_passed { id = 10231023, grade = SOME 0 } = false
val test_has_passed_4 = has_passed { id = 1729, grade = NONE } = false
val test_has_passed_5 = has_passed { id = 432, grade = SOME 74 } = false
val test_has_passed_6 = has_passed { id = 2, grade = SOME 75 } = true
val test_has_passed_7 = has_passed { id = 13, grade = SOME 100 } = true
val test_has_passed_8 = has_passed { id = 15, grade = SOME 86 } = true
	  
(* Pass/Fail -- #3 *)
val test_number_passed_1 = number_passed [] = 0
val test_number_passed_2 = number_passed [{ id = 1, grade = SOME 65 },
					  { id = 2, grade = SOME 82 },
					  { id = 3, grade = NONE },
					  { id = 5, grade = SOME 96 }] = 2
val test_number_passed_3 = number_passed [{ id = 12, grade = SOME 100 },
					  { id = 14, grade = SOME 0 },
					  { id = 9, grade = NONE },
					  { id = 2, grade = NONE }] = 1
val test_number_passed_4 = number_passed [{ id = 1, grade = SOME 76 },
					  { id = 2, grade = SOME 82 },
					  { id = 5, grade = SOME 96 }] = 3


val test_number_misgraded_1 =
    number_misgraded [(pass, {id = 1, grade = SOME 100})] = 0;
val test_number_misgraded_2 =
    number_misgraded ([(pass, {id = 1, grade = SOME 0})]) = 1;
val test_number_misgraded_3 =
    number_misgraded ([(fail, {id = 1, grade = SOME 100})]) = 1;
val test_number_misgraded_4 =
    number_misgraded ([(fail, {id = 1, grade = NONE})]) = 0;
val test_number_misgraaded_5 =
    number_misgraded ([(pass, {id = 1, grade = SOME 100}),
		       (pass, {id = 2, grade = SOME 0}),
		       (fail, {id = 3, grade = SOME 100}),
		       (fail, {id = 4, grade = NONE})]) = 2;									     
(** Forest For The Trees **)

(* Forest For The Trees -- 1 *)

val test_tree_height_1 = tree_height (node { value = 0, left = node { value = 0,
    left = node { value = 0, left = leaf, right = leaf }, right = leaf },
    right = node { value = 0, left = leaf, right = leaf } }) = 3
val test_tree_height_2 = tree_height leaf = 0
val test_tree_height_3 = tree_height (node { value = "abcde", left = leaf, right = leaf }) = 1
val test_tree_height_4 = tree_height (node { value = true, left = leaf, right = leaf }) = 1
val test_tree_height_5 = tree_height (node { value = 0, left = leaf,
    right = node { value = 0, left = node { value = 0, left = leaf, right = leaf }, right = leaf } }) = 3

(* Forest For The Trees -- 2 *)
							
val test_sum_tree_1 = sum_tree (node { value = 1, left = node { value = 2,
    left = node { value = 3, left = leaf, right = leaf }, right = leaf },
    right = node { value = 4, left = leaf, right = leaf } }) = 10
val test_sum_tree_2 = sum_tree leaf = 0
val test_sum_tree_3 = sum_tree (node { value = 1729, left = leaf, right = leaf }) = 1729
val test_sum_tree_4 = sum_tree (node { value = 32, left = leaf,
    right = node { value = ~60, left = node { value = 17, left = leaf, right = leaf }, right = leaf } }) = ~11

(* Forest For The Trees -- 3 *)
										       
val test_gardener_1 = gardener (node { value = leave_me_alone,
    left = node { value = prune_me, left = node { value = leave_me_alone, left = leaf, right = leaf }, right = leaf },
    right = node { value = leave_me_alone, left = leaf, right = leaf } }) =
    node { value = leave_me_alone, left = leaf, right = node { value = leave_me_alone, left = leaf, right = leaf } }
val test_gardener_2 = gardener leaf = leaf
val test_gardener_3 = gardener (node { value = prune_me, left = node { value = prune_me,
    left = node { value = leave_me_alone, left = leaf, right = leaf }, right = leaf },
    right = node { value = leave_me_alone, left = leaf, right = leaf } }) = leaf
val test_gardener_4 = gardener (node { value = leave_me_alone,
    left = node { value = leave_me_alone, left = node { value = leave_me_alone, left = leaf, right = leaf }, right = leaf },
    right = node { value = leave_me_alone, left = leaf, right = leaf } }) =
    node { value = leave_me_alone, left = node { value = leave_me_alone,
    left = node { value = leave_me_alone, left = leaf, right = leaf }, right = leaf },
    right = node { value = leave_me_alone, left = leaf, right = leaf } }
val test_gardener_5 = gardener (node { value = leave_me_alone, left = node { value = leave_me_alone,
    left = node { value = prune_me, left = leaf, right = leaf }, right = leaf },
    right = node { value = prune_me, left = leaf, right = leaf } }) =
    node { value = leave_me_alone, left = node { value = leave_me_alone, left = leaf, right = leaf }, right = leaf }
val test_gardener_6 = gardener (node { value = prune_me, left = leaf, right = leaf }) = leaf
val test_gardener_7 = gardener (node { value = leave_me_alone, left = leaf, right = leaf }) =
    node { value = leave_me_alone, left = leaf, right = leaf }
val test_gardener_8 = gardener (node { value = leave_me_alone, left = leaf,
    right = node { value = prune_me, left = node { value = prune_me, left = leaf, right = leaf }, right = leaf } }) =
    node { value = leave_me_alone, left = leaf, right = leaf }

(* Build SML Library List Functions *)

(* SML Library Last *)
val test_mylast_2 = mylast [1,3,4] = 4;
val test_mylast_2 = mylast [1] = 1;
val test_mylast_3 = mylast [1,2] = 2;
val test_mylast_4 = mylast ["ab", "cd", "ef"] = "ef";
val test_mylast_5 = mylast [(1,2),(3,4), (5,6)] = (5,6);

(* SML Library Take *)
val test_mytake_1 = mytake ([1,2,3], 0) = [];
val test_mytake_2 = mytake ([1,2,3], 2) = [1,2];
val test_mytake_3 = mytake (["ab", "cd", "ef"], 1) = ["ab"];
val test_mytake_4 = mytake ([(1,2), (3,4), (5,6), (7,8)], 4)
		    = [(1,2), (3,4), (5,6), (7,8)];
val test_mytake_5 = mytake ([(1,2), (3,4), (5,6), (7,8)], 3)
		    = [(1,2), (3,4), (5,6)];

(* SML Library Drop *)
val test_mydrop_1 = mydrop ([1,2,3], 2) = [3];
val test_mydrop_2 = mydrop ([1,2,3,4,5,6], 2) = [3,4,5,6];
val test_mydrop_3 = mydrop (["a", "b", "c"], 0) = ["a", "b", "c"];
val test_mydrop_4 = mydrop (["a", "b", "c"], 3) = [];

(* SML Library Concat *)
val test_myconcat_1 = myconcat ([[], [1]]) = [1];
val test_myconcat_2 = myconcat ([[1,2,3], [4,5,6]]) = [1,2,3,4,5,6];
val test_myconcat_3 = myconcat ([[1], [4,5,6]]) = [1,4,5,6];
val test_myconcat_4 = myconcat ([[1,2,3], [4]]) = [1,2,3,4];
val test_myconcat_5 = myconcat ([[1,2,3], [4,5,6], [7,8,9]]) = [1,2,3,4,5,6,7,8,9];
val test_myconcat_6 = myconcat ([["a", "b", "c"], ["d", "e", "f"]])
		      = ["a","b","c","d","e","f"];

(** Natural Numbers **)

(* Natural Numbers -- 9 *)
val test_is_positive_1 = is_positive (ZERO) = false;
val test_is_positive_2 = is_positive (SUCC ZERO) = true ;
val test_is_positive_3 = is_positive (SUCC (SUCC ZERO)) = true;

(* Natural Numbers -- 10 *)
val test_pred_ = pred (SUCC ZERO) = ZERO;
val test_pred_ = pred (SUCC (SUCC ZERO)) = (SUCC ZERO);
val test_pred_ = pred (SUCC (SUCC (SUCC ZERO))) = (SUCC (SUCC ZERO));

(* Natural Numbers -- 11 *)
val test_nat_to_int_1 = nat_to_int (ZERO) = 0;
val test_nat_to_int_2 = nat_to_int (SUCC ZERO) = 1;
val test_nat_to_int_3 = nat_to_int (SUCC (SUCC ZERO)) = 2;
val test_nat_to_int_4 = nat_to_int (SUCC (SUCC (SUCC ZERO))) = 3;

(* Natural Numbers -- 12 *)
val test_int_to_nat = int_to_nat (0) = ZERO;
val test_int_to_nat = int_to_nat (1) = SUCC ZERO;
val test_int_to_nat = int_to_nat (2) = SUCC (SUCC ZERO);
val test_int_to_nat = int_to_nat (~2)
		      = ZERO andalso false handle Negative => true | _ => false;
val test_int_to_nat = int_to_nat (~100)
		      = ZERO andalso false handle Negative => true | _ => false;

(* Natural Numbers -- 13 *)
val test_add_1 = add (ZERO, ZERO) = ZERO;
val test_add_2 = add ((SUCC ZERO), (ZERO)) = (SUCC ZERO);
val test_add_3 = add ((SUCC ZERO), (SUCC ZERO)) = (SUCC (SUCC ZERO));
val test_add_4 = add ((SUCC (SUCC ZERO)), (SUCC (SUCC ZERO)))
		 = (SUCC (SUCC (SUCC (SUCC ZERO))));
val test_add_5
    = add ((SUCC (SUCC (SUCC (SUCC ZERO)))), (SUCC (SUCC (SUCC (SUCC ZERO)))))
      = (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC ZERO))))))));

(* Natural Numbers -- 14 *)
val test_sub = sub (ZERO, ZERO) = ZERO;
val test_sub = sub ((SUCC ZERO), ZERO) = (SUCC ZERO);
val test_sub = sub ((SUCC (SUCC ZERO)), (SUCC ZERO)) = (SUCC ZERO);
val test_sub = sub ((SUCC (SUCC (SUCC ZERO))), (SUCC ZERO)) = (SUCC (SUCC ZERO));

(* Natural Numbers -- 16 *)
val test_less_than_1 = less_than (ZERO, ZERO) = false;
val test_less_than_2 = less_than (ZERO, (SUCC ZERO)) = true;
val test_less_than_3 = less_than ((SUCC ZERO), ZERO) = false;
val test_less_than_4 = less_than ((SUCC (SUCC ZERO)), SUCC ZERO) = false;


















(*
(** Back To The Future! **)

(* GCD -- Redux *)
val test_gcd_list_1 = gcd_list [18, 12, 3] = 3
val test_gcd_list_2 = gcd_list [18] = 18
val test_gcd_list_3 = gcd_list [18, 12, 13] = 1
val test_gcd_list_4 = gcd_list [10, 18, 12] = 2
val test_gcd_list_5 = gcd_list [100, 1000, 1] = 1
val test_gcd_list_6 = gcd_list [18, 12, 180, 42] = 6
val test_gcd_list_7 = gcd_list [18, 12] = 6

(* Element Of A List -- Redux *)
val test_any_divisible_by_1 = any_divisible_by ([13, 1, 20], 5) = true
val test_any_divisible_by_2 = any_divisible_by ([13, 1, 20], 3) = false
val test_any_divisible_by_3 = any_divisible_by ([], 5) = false
val test_any_divisible_by_4 = any_divisible_by ([13, 1, 20], 13) = true
val test_any_divisible_by_5 = any_divisible_by ([13, 1, 20], 12) = false
val test_any_divisible_by_6 = any_divisible_by ([13, 1, 20], 1) = true

(* Quirky Addition -- Redux *)
val test_add_opt_1 = add_opt (SOME 1, SOME 2) = SOME 3
val test_add_opt_2 = add_opt (SOME 1, NONE) = NONE
val test_add_opt_3 = add_opt (NONE, SOME 2) = NONE
val test_add_opt_4 = add_opt (NONE, NONE) = NONE
val test_add_opt_5 = add_opt (SOME ~123, SOME 15) = SOME ~108

(* Quirky Addition -- Continued -- Redux *)
val test_add_all_opt_1 = add_all_opt [SOME 1, NONE, SOME 3] = SOME 4
val test_add_all_opt_2 = add_all_opt [] = NONE
val test_add_all_opt_3 = add_all_opt [NONE, NONE, NONE] = NONE
val test_add_all_opt_4 = add_all_opt [SOME 123] = SOME 123
val test_add_all_opt_5 = add_all_opt [NONE, SOME ~1, NONE, NONE] = SOME ~1

(* Flip Flop -- Redux *)
val test_alternate_1 = alternate [1, 2, 3, 4] = ~2
val test_alternate_2 = alternate [] = 0
val test_alternate_3 = alternate [~100] = ~100
val test_alternate_4 = alternate [1, ~2, 3, ~4] = 10
val test_alternate_5 = alternate [~1, 2, ~3, 4] = ~10

(* Minimum/Maximum -- Redux *)
val test_min_max_1 = min_max [3, 1, 2, 5, 4] = (1, 5)
val test_min_max_2 = min_max [1] = (1, 1)
val test_min_max_3 = min_max [~1000000, 1, 1, 1, 1000000] = (~1000000, 1000000)

(* Lists And Tuples, Oh My! -- Redux *)
val test_unzip_1 = unzip [(1, 2), (3, 4), (5, 6)] = ([1, 3, 5], [2, 4, 6])
(* causes polyEqual warning if unzip has a polymorphic type -- that's ok *)
val test_unzip_2 = unzip [] = ([], [])
val test_unzip_3 = unzip [(123, 321), (321, 123)] = ([123, 321], [321, 123])

(* BBCA -- Redux *)
val test_repeats_list_1 = repeats_list (["abc", "def", "ghi"], [4, 0, 3]) =
    ["abc", "abc", "abc", "abc", "ghi", "ghi", "ghi"]
(* causes polyEqual warning if repeats_list has a polymorphic type -- that's ok *)
val test_repeats_list_2 = repeats_list ([], []) = []
val test_repeats_list_3 = repeats_list (["a"], [10]) = ["a", "a", "a", "a", "a", "a", "a", "a", "a", "a"]
									 
									  
*)
