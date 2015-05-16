type data =
	|Int of int 
	|String of string
	|Float of float
	|Char of char
;;

let quickSort list =
	let compare a b =
		match a,b with
		| Int x, Int y ->
			if x = y then 0
			else if x > y then 1
			else -1 
		| Float x, Float y -> 
			if x = y then 0
			else if x > y then 1
			else -1 
		| Char x, Char y ->
			Char.compare x y
		| String x, String y ->
			String.compare x y
		| _ , _ -> failwith "type does not match"
	in
	(*the really sort algorithm*)
	let rec sort l =
		match l with
		| [] -> []
		| [x] -> [x]
		| base :: rest ->
			(*First, divide the rest elements accorrding to base *)
			let rec divide l left right base = 
				(*put the elements smaller than base into left
				and put the elements bigger than base into right.*)
				match l with
				(*Second, when the first step is done, then sort the left and right sublists*)
				| [] -> 
					let leftList = sort left in
					let rightList = sort right in
					(*Finally, combine left and right sublists together as a sorted list*)
					leftList @ (base::rightList)
				(* divide elements *)
				| h :: tail -> 
					if (compare h base) < 0 then divide tail (h :: left) right base
					else divide tail left (h::right) base
			in 
			divide rest [] [] base
	in 
	sort list
;;

(*Test*)
let print_list list =
	let print_data d =
		match d with
		| Int x -> print_int x; print_string ";"
		| Float x -> print_float x; print_string ";"
		| Char x -> print_char x; print_string ";"
		| String x -> print_string x; print_string ";"
	in
	print_string "[";
	List.iter print_data list;
	print_endline "]"
;;

(* expected: [1;2;3;4;5;] *)
let list1 = [Int 5;Int 4;Int 3; Int 2; Int 1];;
let result1 = quickSort list1;;
print_list result1;;

(* expected: [1.23;2.23;3.23;4.23;5.23;] *)
let list2 = [Float 1.23; Float 2.23; Float 3.23; Float 4.23; Float 5.23];;
let result2 = quickSort list2;;
print_list result2;;

(* expected: [a;c;e;s;z;]*)
let list3 = [Char 'e'; Char 'a'; Char 'z'; Char 's'; Char 'c'];;
let result3 = quickSort list3;;
print_list result3;;

(* expected: [OCaml;Swift;Unity;li;ye;] *)
let list4 = [String "li"; String "ye"; String "OCaml"; String "Unity"; String "Swift"];;
let result4 = quickSort list4;;
print_list result4;;
